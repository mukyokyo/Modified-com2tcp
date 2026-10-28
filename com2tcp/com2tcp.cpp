/*
 * $Id: com2tcp.cpp,v 1.10 2006/11/16 12:51:43 vfrolov Exp $
 *
 * Copyright (c) 2005-2006 Vyacheslav Frolov
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 * $Log: com2tcp.cpp,v $
 * Revision 1.32  2026/10/28
 * Restructured into options, COM port, LSRMST/PUSR translation, sockets and
 * the transfer session. Fixed:
 *  - fInX was always on, so the driver put XOFF/XON into the paired port's
 *    data whenever our input buffer filled up. Flow control is now --flow
 *    and off by default
 *  - crash in raw mode without --terminal (strcmp() on NULL)
 *  - the current LSRMST settings were sent with the decoded length (garbage
 *    bytes after them), and twice in PUSR mode
 *  - crash in PUSR mode when the driver refused the insertion, and the
 *    insertion objects leaked on every connection
 *  - overlapped I/O was cancelled but not waited for, so the system could
 *    still write into the freed stack frame
 *  - data still in the socket was dropped when the peer closed
 *  - Ctrl+C (and nssm's stop) took up to 5 s, and blocked in accept()
 *  - Win32 error codes were printed with strerror()
 * Added --flow, --lsrmst and --pusr (--terminal lsrmst|pusr still work).
 * Bigger transfer buffers, socket options also in server mode.
 *  Copyright (c) 2026 BestTechnology CO.,LTD.
 *
 * Revision 1.31  2026/01/27 m.miyashige & t.uemitsu
 * Added support IOCTL_SERIAL_LSRMST_INSERT and PUSR's baudrate/line control update packets
 *  Copyright (c) 2026 BestTechnology CO.,LTD.
 *
 * Revision 1.10  2006/11/16 12:51:43  vfrolov
 * Added ability to set COM port parameters
 *
 * Revision 1.9  2005/11/25 13:49:23  vfrolov
 * Implemented --interface option for client mode
 *
 * Revision 1.8  2005/10/18 09:53:36  vfrolov
 * Added EVENT_ACCEPT
 *
 * Revision 1.7  2005/10/03 13:48:08  vfrolov
 * Added --ignore-dsr and listen options
 *
 * Revision 1.6  2005/06/10 15:55:10  vfrolov
 * Implemented --terminal option
 *
 * Revision 1.5  2005/06/08 15:48:17  vfrolov
 * Implemented --awak-seq option
 *
 * Revision 1.4  2005/06/07 10:06:37  vfrolov
 * Added ability to use port names
 *
 * Revision 1.3  2005/06/06 15:20:46  vfrolov
 * Implemented --telnet option
 *
 * Revision 1.2  2005/05/30 12:17:32  vfrolov
 * Fixed resolving problem
 *
 * Revision 1.1  2005/05/30 10:02:33  vfrolov
 * Initial revision
 *
 */

#include "precomp.h"
#include "telnet.h"
#include "cncext.h"
#include "version.h"

#include <ws2tcpip.h>
#include <mstcpip.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

// Size of one read or write on either side. A read returns as soon as there is
// anything at all, so this only limits how much one call can carry.
static const int kChunk = 16384;

// How long the writes that are under way may still take when a session ends
static const DWORD kLingerMs = 1000;

// Escape character for the LSRMST insertion (see EnableLsrMst()). With
// --lsrmst the host has to decode the stream with the same character.
static const BYTE kLsrMstEscape = 'a';

///////////////////////////////////////////////////////////////
// Logging. Every line is flushed at once, so nothing is lost when the output
// goes to a file or a service log.

static void LogLine(FILE *out, const char *pFmt, va_list va, const char *pTail)
{
  SYSTEMTIME st;
  GetLocalTime(&st);
  fprintf(out, "%04d-%02d-%02d %02d:%02d:%02d.%03d ",
          st.wYear, st.wMonth, st.wDay, st.wHour, st.wMinute, st.wSecond, st.wMilliseconds);
  vfprintf(out, pFmt, va);
  if (pTail)
    fputs(pTail, out);
  fputc('\n', out);
  fflush(out);
}

static void Log(const char *pFmt, ...)
{
  va_list va;
  va_start(va, pFmt);
  LogLine(stdout, pFmt, va, NULL);
  va_end(va);
}

// err is a Win32 or Winsock error code
static void LogError(DWORD err, const char *pFmt, ...)
{
  char msg[256];
  DWORD len = FormatMessageA(FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
                             NULL, err, 0, msg, sizeof(msg), NULL);
  while (len && (msg[len - 1] == '\r' || msg[len - 1] == '\n' || msg[len - 1] == ' ' || msg[len - 1] == '.'))
    msg[--len] = '\0';
  if (!len)
    strcpy(msg, "unknown error");

  char tail[320];
  snprintf(tail, sizeof(tail), " - ERROR %lu: %s", (unsigned long)err, msg);

  va_list va;
  va_start(va, pFmt);
  LogLine(stderr, pFmt, va, tail);
  va_end(va);
}

#define LOG_LAST_ERROR(...) LogError(GetLastError(), __VA_ARGS__)
#define LOG_WSA_ERROR(...)  LogError((DWORD)WSAGetLastError(), __VA_ARGS__)

///////////////////////////////////////////////////////////////
// Ctrl+C, Ctrl+Break and closing the console end the program (nssm stops a
// service with Ctrl+C). Every wait below also waits for this event, so the
// program reacts at once.

static HANDLE hStopEvent = NULL;

static BOOL WINAPI OnConsoleCtrl(DWORD)
{
  SetEvent(hStopEvent);
  return TRUE;
}

static bool StopRequested()
{
  return WaitForSingleObject(hStopEvent, 0) == WAIT_OBJECT_0;
}

// Sleeps; false when the program is being stopped
static bool Pause(DWORD ms)
{
  return WaitForSingleObject(hStopEvent, ms) == WAIT_TIMEOUT;
}

///////////////////////////////////////////////////////////////
// Command line

enum FlowControl { flowNone, flowXonXoff, flowRtsCts };
enum Insertion { insNone, insLsrMst, insPusr };

struct Options
{
  bool telnet = false;
  const char *pTermType = NULL;
  const BYTE *pAwakSeq = NULL;
  const char *pIF = NULL;
  FlowControl flow = flowNone;
  Insertion insertion = insNone;
  ComParams com;

  const char *pPath = NULL;
  const char *pHost = NULL;     // NULL in server mode
  const char *pPort = NULL;
};

static const char *FlowStr(FlowControl flow)
{
  switch (flow) {
    case flowXonXoff: return "xonxoff";
    case flowRtsCts: return "rtscts";
    default: return "none";
  }
}

// false when the command line cannot be used (the caller shows the usage)
static bool ParseArgs(int argc, char *argv[], Options &opt)
{
  int i = 1;

  for (; i < argc && argv[i][0] == '-' ; i++) {
    const char *pName = argv[i];

    if (!strcmp(pName, "--help"))
      return false;

    if (!strcmp(pName, "--telnet")) {
      opt.telnet = true;
      continue;
    }
    if (!strcmp(pName, "--ignore-dsr")) {
      opt.com.SetIgnoreDSR(TRUE);
      continue;
    }
    if (!strcmp(pName, "--lsrmst")) {
      opt.insertion = insLsrMst;
      continue;
    }
    if (!strcmp(pName, "--pusr")) {
      opt.insertion = insPusr;
      continue;
    }

    // the rest take a value
    if (i + 1 >= argc) {
      fprintf(stderr, "Missing value for %s\n", pName);
      return false;
    }
    const char *pValue = argv[++i];

    if (!strcmp(pName, "--terminal")) {
      opt.pTermType = pValue;
    } else if (!strcmp(pName, "--baud")) {
      opt.com.SetBaudRate(pValue);
    } else if (!strcmp(pName, "--data")) {
      opt.com.SetByteSize(pValue);
    } else if (!strcmp(pName, "--parity")) {
      if (!opt.com.SetParity(pValue)) {
        fprintf(stderr, "Unknown parity value %s\n", pValue);
        return false;
      }
    } else if (!strcmp(pName, "--stop")) {
      if (!opt.com.SetStopBits(pValue)) {
        fprintf(stderr, "Unknown stop bits value %s\n", pValue);
        return false;
      }
    } else if (!strcmp(pName, "--awak-seq")) {
      opt.pAwakSeq = (const BYTE *)pValue;
    } else if (!strcmp(pName, "--interface")) {
      opt.pIF = pValue;
    } else if (!strcmp(pName, "--flow")) {
      if (!strcmp(pValue, "none"))
        opt.flow = flowNone;
      else if (!strcmp(pValue, "xonxoff"))
        opt.flow = flowXonXoff;
      else if (!strcmp(pValue, "rtscts"))
        opt.flow = flowRtsCts;
      else {
        fprintf(stderr, "Unknown flow control %s\n", pValue);
        return false;
      }
    } else {
      fprintf(stderr, "Unknown option %s\n", pName);
      return false;
    }
  }

  int rest = argc - i;

  if (rest != 2 && rest != 3)
    return false;

  opt.pPath = argv[i];
  if (rest == 3) {
    opt.pHost = argv[i + 1];
    opt.pPort = argv[i + 2];
  } else {
    opt.pPort = argv[i + 1];
  }

  // older scripts choose the insertion with --terminal in raw mode
  if (!opt.telnet && opt.pTermType && opt.insertion == insNone) {
    if (!strcmp(opt.pTermType, "lsrmst"))
      opt.insertion = insLsrMst;
    else if (!strcmp(opt.pTermType, "pusr"))
      opt.insertion = insPusr;
  }

  return true;
}

static void Usage(const char *pProgName)
{
  fprintf(stderr, "Usage (client mode):\n");
  fprintf(stderr, "    %s [options] \\\\.\\<com port> <host addr> <host port>\n", pProgName);
  fprintf(stderr, "\n");
  fprintf(stderr, "Usage (server mode):\n");
  fprintf(stderr, "    %s [options] \\\\.\\<com port> <listen port>\n", pProgName);
  fprintf(stderr, "\n");
  fprintf(stderr, "Common options:\n");
  fprintf(stderr, "    --telnet              - use Telnet protocol.\n");
  fprintf(stderr, "    --terminal <type>     - use terminal <type> (RFC 1091). Without --telnet,\n");
  fprintf(stderr, "                            lsrmst and pusr mean --lsrmst and --pusr.\n");
  fprintf(stderr, "    --lsrmst              - pass the paired port's settings (com0com LSRMST\n");
  fprintf(stderr, "                            insertion, escape character '%c') on to the host.\n", kLsrMstEscape);
  fprintf(stderr, "    --pusr                - turn the paired port's settings into USR-TCP232\n");
  fprintf(stderr, "                            (PUSR) configuration packets for the host.\n");
  fprintf(stderr, "    --help                - show this help.\n");
  fprintf(stderr, "\n");
  fprintf(stderr, "COM port options:\n");
  fprintf(stderr, "    --baud <b>            - set baud rate to <b> (default is %ld),\n",
                                               (long)ComParams().BaudRate());
  fprintf(stderr, "                            where <b> is %s.\n",
                                               ComParams::BaudRateLst());
  fprintf(stderr, "    --data <d>            - set data bits to <d> (default is %ld), where <d> is\n",
                                               (long)ComParams().ByteSize());
  fprintf(stderr, "                            %s.\n",
                                               ComParams::ByteSizeLst());
  fprintf(stderr, "    --parity <p>          - set parity to <p> (default is %s), where <p> is\n",
                                               ComParams::ParityStr(ComParams().Parity()));
  fprintf(stderr, "                            %s.\n",
                                               ComParams::ParityLst());
  fprintf(stderr, "    --stop <s>            - set stop bits to <s> (default is %s), where <s> is\n",
                                               ComParams::StopBitsStr(ComParams().StopBits()));
  fprintf(stderr, "                            %s.\n",
                                               ComParams::StopBitsLst());
  fprintf(stderr, "    --flow <f>            - flow control towards the paired port: none (default),\n");
  fprintf(stderr, "                            xonxoff or rtscts.\n");
  fprintf(stderr, "    --ignore-dsr          - ignore DSR state (do not wait DSR to be ON before\n");
  fprintf(stderr, "                            connecting to host, do not close connection after\n");
  fprintf(stderr, "                            DSR is OFF and do not ignore any bytes received\n");
  fprintf(stderr, "                            while DSR is OFF).\n");
  fprintf(stderr, "\n");
  fprintf(stderr, "    The value d[efault] above means to use current COM port settings.\n");
  fprintf(stderr, "\n");
  fprintf(stderr, "Client mode options:\n");
  fprintf(stderr, "    --awak-seq <sequence> - wait for awakening <sequence> from com port\n"
                  "                            before connecting to host. All data before\n"
                  "                            <sequence> and <sequence> itself will not be sent.\n");
  fprintf(stderr, "    --interface <if>      - use interface <if> for connecting.\n");
  fprintf(stderr, "\n");
  fprintf(stderr, "Server mode options:\n");
  fprintf(stderr, "    --interface <if>      - use interface <if> for listening.\n");
  exit(1);
}

///////////////////////////////////////////////////////////////
// One overlapped operation. The system writes into the OVERLAPPED (and into
// the buffer) until the operation is over, so neither may go away while it is
// pending: whoever cancels has to wait with WaitCancelled().

class AsyncOp
{
  public:
    enum Result { done, aborted, failed };

    AsyncOp() : hFile(INVALID_HANDLE_VALUE), pending(false) { memset(&ov, 0, sizeof(ov)); }
    ~AsyncOp() { if (ov.hEvent) CloseHandle(ov.hEvent); }

    bool Init(HANDLE h)
    {
      hFile = h;
      ov.hEvent = CreateEvent(NULL, TRUE, FALSE, NULL);
      if (!ov.hEvent)
        LOG_LAST_ERROR("CreateEvent()");
      return ov.hEvent != NULL;
    }

    HANDLE Event() const { return ov.hEvent; }
    OVERLAPPED *Ov() { return &ov; }
    bool Pending() const { return pending; }

    // ok is what ReadFile()/WriteFile()/WaitCommEvent() returned
    bool Started(BOOL ok, const char *pWhat)
    {
      if (!ok && GetLastError() != ERROR_IO_PENDING) {
        LOG_LAST_ERROR("%s", pWhat);
        return false;
      }
      pending = true;
      return true;
    }

    // once the event is signaled
    Result Finish(DWORD &bytes, const char *pWhat)
    {
      bytes = 0;
      BOOL ok = GetOverlappedResult(hFile, &ov, &bytes, FALSE);
      DWORD err = ok ? 0 : GetLastError();

      pending = false;
      ResetEvent(ov.hEvent);

      if (ok)
        return done;

      bytes = 0;
      // cancelled, or a serial read that ran into its timeout: nothing
      // happened, the operation is simply started again
      if (err == ERROR_OPERATION_ABORTED || err == ERROR_TIMEOUT)
        return aborted;

      LogError(err, "%s", pWhat);
      return failed;
    }

    // after CancelIo() on the handle
    void WaitCancelled()
    {
      if (!pending)
        return;

      DWORD bytes;
      GetOverlappedResult(hFile, &ov, &bytes, TRUE);
      pending = false;
      ResetEvent(ov.hEvent);
    }

  private:
    HANDLE hFile;
    OVERLAPPED ov;
    bool pending;
};

// DeviceIoControl() on a handle opened for overlapped I/O, waiting for the result
static bool SyncIoctl(HANDLE h, DWORD code, void *pIn, DWORD inSize, void *pOut, DWORD outSize, DWORD *pReturned)
{
  OVERLAPPED ov;
  memset(&ov, 0, sizeof(ov));
  ov.hEvent = CreateEvent(NULL, TRUE, FALSE, NULL);
  if (!ov.hEvent) {
    LOG_LAST_ERROR("CreateEvent()");
    return false;
  }

  *pReturned = 0;
  BOOL ok = DeviceIoControl(h, code, pIn, inSize, pOut, outSize, pReturned, &ov);
  if (!ok && GetLastError() == ERROR_IO_PENDING)
    ok = GetOverlappedResult(h, &ov, pReturned, TRUE);

  DWORD err = GetLastError();
  CloseHandle(ov.hEvent);
  SetLastError(err);
  return ok != FALSE;
}

///////////////////////////////////////////////////////////////
// COM port

static bool ConfigureComPort(HANDLE hCom, const Options &opt)
{
  const ComParams &cp = opt.com;
  DCB dcb;

  memset(&dcb, 0, sizeof(dcb));
  dcb.DCBlength = sizeof(dcb);

  if (!GetCommState(hCom, &dcb)) {
    LOG_LAST_ERROR("GetCommState()");
    return false;
  }

  // d[efault] keeps what the port already has
  if (cp.BaudRate() > 0)
    dcb.BaudRate = (DWORD)cp.BaudRate();
  if (cp.ByteSize() > 0)
    dcb.ByteSize = (BYTE)cp.ByteSize();
  if (cp.Parity() >= 0)
    dcb.Parity = (BYTE)cp.Parity();
  if (cp.StopBits() >= 0)
    dcb.StopBits = (BYTE)cp.StopBits();

  // With fInX the driver itself sends XOFF/XON to the other side whenever our
  // input buffer gets full/empty -- right in the middle of the data the paired
  // port receives. It used to be always on; now only when asked for.
  dcb.fInX = (opt.flow == flowXonXoff);
  dcb.fOutX = (opt.flow == flowXonXoff);
  dcb.XonChar = 0x11;
  dcb.XoffChar = 0x13;
  dcb.XonLim = 100;
  dcb.XoffLim = 100;

  dcb.fOutxCtsFlow = (opt.flow == flowRtsCts);
  dcb.fRtsControl = (opt.flow == flowRtsCts) ? RTS_CONTROL_HANDSHAKE : RTS_CONTROL_ENABLE;
  dcb.fOutxDsrFlow = FALSE;
  dcb.fDsrSensitivity = !cp.IgnoreDSR();
  dcb.fDtrControl = DTR_CONTROL_ENABLE;

  dcb.fParity = FALSE;
  dcb.fNull = FALSE;
  dcb.fAbortOnError = FALSE;
  dcb.fErrorChar = FALSE;

  if (!SetCommState(hCom, &dcb)) {
    LOG_LAST_ERROR("SetCommState()");
    return false;
  }

  // A read returns as soon as there is at least one byte (with all there is,
  // up to the buffer size); a write has no time limit
  COMMTIMEOUTS timeouts;

  memset(&timeouts, 0, sizeof(timeouts));
  timeouts.ReadIntervalTimeout = MAXDWORD;
  timeouts.ReadTotalTimeoutMultiplier = MAXDWORD;
  timeouts.ReadTotalTimeoutConstant = MAXDWORD - 1;
  timeouts.WriteTotalTimeoutMultiplier = 0;
  timeouts.WriteTotalTimeoutConstant = 0;

  if (!SetCommTimeouts(hCom, &timeouts)) {
    LOG_LAST_ERROR("SetCommTimeouts()");
    return false;
  }

  Log("OpenComPort(\"%s\", baud=%ld, data=%ld, parity=%s, stop=%s, flow=%s) - OK",
      opt.pPath,
      (long)dcb.BaudRate,
      (long)dcb.ByteSize,
      ComParams::ParityStr(dcb.Parity),
      ComParams::StopBitsStr(dcb.StopBits),
      FlowStr(opt.flow));
  return true;
}

static HANDLE OpenComPort(const Options &opt)
{
  HANDLE hCom = CreateFileA(opt.pPath, GENERIC_READ | GENERIC_WRITE, 0, NULL,
                            OPEN_EXISTING, FILE_FLAG_OVERLAPPED, NULL);

  if (hCom == INVALID_HANDLE_VALUE) {
    LOG_LAST_ERROR("OpenComPort(): CreateFile(\"%s\")", opt.pPath);
    return INVALID_HANDLE_VALUE;
  }

  if (!ConfigureComPort(hCom, opt)) {
    CloseHandle(hCom);
    return INVALID_HANDLE_VALUE;
  }
  return hCom;
}

///////////////////////////////////////////////////////////////
// com0com can insert the paired port's settings into the data we read from
// it (IOCTL_SERIAL_LSRMST_INSERT with com0com's extension, see tstser.cpp at
// http://com0com.sourceforge.net/examples/MODEM_CONTROL/tstser.cpp).
// Every insertion starts with the escape character; the escape character
// in the data itself arrives as <esc> SERIAL_LSRMST_ESCAPE.
//
//   --lsrmst  passes this stream on as it is (the host decodes it; it has to
//             use the same escape character, kLsrMstEscape)
//   --pusr    decodes it here and turns new settings into the configuration
//             packets of the USR-TCP232 series (PUSR)

static bool LsrMstIoctl(HANDLE hCom, BYTE escapeChar, ULONG options,
                        BYTE *pOut, DWORD outSize, DWORD *pReturned)
{
  BYTE in[sizeof(UCHAR) + C0CE_SIGNATURE_SIZE + sizeof(ULONG)];

  in[0] = escapeChar;
  memcpy(in + sizeof(UCHAR), C0CE_SIGNATURE, C0CE_SIGNATURE_SIZE);
  memcpy(in + sizeof(UCHAR) + C0CE_SIGNATURE_SIZE, &options, sizeof(options));

  return SyncIoctl(hCom, IOCTL_SERIAL_LSRMST_INSERT, in, sizeof(in), pOut, outSize, pReturned);
}

// Turns the insertion on. current receives the settings as they are now, in
// the same escaped form as later changes. false if the driver refused.
static bool EnableLsrMst(HANDLE hCom, BYTE_vector &current)
{
  const ULONG wanted = C0CE_INSERT_IOCTL_GET | C0CE_INSERT_ENABLE_RBR | C0CE_INSERT_ENABLE_RLC;
  ULONG options = C0CE_INSERT_ENABLE_LSR | C0CE_INSERT_ENABLE_MST;   // what any serial driver can do

  BYTE caps[C0CE_SIGNATURE_SIZE + sizeof(ULONG)];
  DWORD returned;

  if (!LsrMstIoctl(hCom, 0, C0CE_INSERT_IOCTL_CAPS, caps, sizeof(caps), &returned)) {
    LOG_LAST_ERROR("IOCTL_SERIAL_LSRMST_INSERT");
  } else if (returned >= sizeof(caps) && !memcmp(caps, C0CE_SIGNATURE, C0CE_SIGNATURE_SIZE)) {
    ULONG supported;

    memcpy(&supported, caps + C0CE_SIGNATURE_SIZE, sizeof(supported));
    Log("The driver supports 0x%lX extended LSRMST_INSERT capabilities", (unsigned long)supported);
    options = supported & wanted;
  } else {
    Log("The driver does not support extended LSRMST_INSERT capabilities");
  }

  if (options == wanted)
    Log("OK, will do 0x%lX", (unsigned long)options);
  else
    Log("FAIL, will do 0x%lX instead of 0x%lX", (unsigned long)options, (unsigned long)wanted);

  // with IOCTL_GET the driver returns the current values, each as <esc> <code> <value>
  DWORD size = 0;

  if (options & C0CE_INSERT_IOCTL_GET) {
    if (options & C0CE_INSERT_ENABLE_LSR)
      size += 2 + sizeof(UCHAR);
    if (options & C0CE_INSERT_ENABLE_MST)
      size += 2 + sizeof(UCHAR);
    if (options & C0CE_INSERT_ENABLE_RBR)
      size += 2 + sizeof(ULONG);
    if (options & C0CE_INSERT_ENABLE_RLC)
      size += 2 + sizeof(UCHAR) * 3;
  }

  current.assign(size, 0);

  if (!LsrMstIoctl(hCom, kLsrMstEscape, options, size ? &current[0] : NULL, size, &returned)) {
    LOG_LAST_ERROR("IOCTL_SERIAL_LSRMST_INSERT");
    current.clear();
    return false;
  }

  current.resize(returned < size ? returned : size);
  return true;
}

class PusrTranslator
{
  public:
    PusrTranslator()
      : state(stData), code(0), need(0), got(0),
        baudRate(115200), byteSize(8), parity(NOPARITY), stopBits(ONESTOPBIT) {}

    // Appends the data without the insertions to out, and a PUSR packet for
    // every change of the settings
    void Translate(const BYTE *p, int len, BYTE_vector &out);

  private:
    static int PayloadSize(BYTE c);
    void Apply(BYTE_vector &out);
    void PutPacket(BYTE_vector &out) const;

    enum { stData, stCode, stPayload } state;
    BYTE code;
    int need;
    int got;
    BYTE payload[sizeof(ULONG)];

    ULONG baudRate;
    int byteSize;
    int parity;
    int stopBits;
};

// bytes after <esc> <code>, or -1 for an unknown code
int PusrTranslator::PayloadSize(BYTE c)
{
  switch (c) {
    case SERIAL_LSRMST_LSR_DATA: return 2;      // LSR, the faulty byte
    case SERIAL_LSRMST_LSR_NODATA: return 1;    // LSR
    case SERIAL_LSRMST_MST: return 1;           // MSR
    case C0CE_INSERT_RBR: return sizeof(ULONG); // baud rate
    case C0CE_INSERT_RLC: return 3;             // byte size, parity, stop bits
  }
  return -1;
}

void PusrTranslator::Translate(const BYTE *p, int len, BYTE_vector &out)
{
  for (int i = 0 ; i < len ; i++) {
    BYTE ch = p[i];

    switch (state) {
      case stData:
        if (ch == kLsrMstEscape)
          state = stCode;
        else
          out.push_back(ch);
        break;

      case stCode:
        code = ch;
        got = 0;
        need = PayloadSize(ch);
        if (code == SERIAL_LSRMST_ESCAPE) {
          out.push_back(kLsrMstEscape);
          state = stData;
        } else {
          state = (need > 0) ? stPayload : stData;   // an unknown code is dropped
        }
        break;

      case stPayload:
        payload[got++] = ch;
        if (got == need) {
          Apply(out);
          state = stData;
        }
        break;
    }
  }
}

// LSR and MST are only consumed; the faulty byte of LSR_DATA is dropped
void PusrTranslator::Apply(BYTE_vector &out)
{
  switch (code) {
    case C0CE_INSERT_RBR:
      memcpy(&baudRate, payload, sizeof(baudRate));
      Log("BaudRate=%lu", (unsigned long)baudRate);
      PutPacket(out);
      break;
    case C0CE_INSERT_RLC:
      byteSize = payload[0];
      parity = payload[1];
      stopBits = payload[2];
      Log("ByteSize=%d Parity=%d StopBits=%d", byteSize, parity, stopBits);
      PutPacket(out);
      break;
  }
}

static int Clamp(int v, int lo, int hi)
{
  return v < lo ? lo : (v > hi ? hi : v);
}

// PUSR's own packet for the baud rate and line control
//  https://www.pusr.com/download/T24/USR-TCP232-T24-EN%20V3.2.5.pdf
//   55 AA 55 <baud: 3 bytes, MSB first> <line> <sum of the 4 bytes before>
//   line: bit0-1 data bits (0:5 .. 3:8), bit2 stop bits (0:1 1:2),
//         bit3-5 parity (0:none 1:odd 3:even 5:mark 7:space)
void PusrTranslator::PutPacket(BYTE_vector &out) const
{
  static const BYTE parityCode[] = {0, 1, 3, 5, 7};   // NOPARITY .. SPACEPARITY

  BYTE dataBits = (BYTE)Clamp(byteSize - 5, 0, 3);
  BYTE twoStops = (stopBits == ONESTOPBIT) ? 0 : 1;   // 1.5 counts as 2
  BYTE parityBits = parityCode[Clamp(parity, 0, 4)];

  BYTE pkt[8];

  pkt[0] = 0x55;
  pkt[1] = 0xAA;
  pkt[2] = 0x55;
  pkt[3] = (BYTE)((baudRate >> 16) & 0xFF);
  pkt[4] = (BYTE)((baudRate >> 8) & 0xFF);
  pkt[5] = (BYTE)(baudRate & 0xFF);
  pkt[6] = (BYTE)(dataBits | (twoStops << 2) | (parityBits << 3));
  pkt[7] = (BYTE)(pkt[3] + pkt[4] + pkt[5] + pkt[6]);

  out.insert(out.end(), pkt, pkt + sizeof(pkt));
}

///////////////////////////////////////////////////////////////
// Sockets (IPv4)

static const char *AddrStr(const sockaddr *pAddr, char *pBuf, size_t size)
{
  const sockaddr_in *pIn = (const sockaddr_in *)pAddr;
  unsigned long a = ntohl(pIn->sin_addr.s_addr);

  snprintf(pBuf, size, "%lu.%lu.%lu.%lu:%u",
           (a >> 24) & 0xFF, (a >> 16) & 0xFF, (a >> 8) & 0xFF, a & 0xFF,
           (unsigned)ntohs(pIn->sin_port));
  return pBuf;
}

// pHost NULL is any address; the port may be a number or a service name
static addrinfo *Resolve(const char *pHost, const char *pPort, bool passive)
{
  addrinfo hints;

  memset(&hints, 0, sizeof(hints));
  hints.ai_family = AF_INET;
  hints.ai_socktype = SOCK_STREAM;
  hints.ai_protocol = IPPROTO_TCP;
  if (passive)
    hints.ai_flags = AI_PASSIVE;

  addrinfo *pList = NULL;
  int err = getaddrinfo(pHost, pPort, &hints, &pList);

  if (err) {
    LogError((DWORD)err, "getaddrinfo(\"%s\", \"%s\")", pHost ? pHost : "*", pPort);
    return NULL;
  }
  return pList;
}

// A socket, bound to the interface and/or port if given
static SOCKET NewSocket(const char *pIF, const char *pPort)
{
  SOCKET hSock = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);

  if (hSock == INVALID_SOCKET) {
    LOG_WSA_ERROR("socket()");
    return INVALID_SOCKET;
  }

  if (pIF || pPort) {
    addrinfo *pAddr = Resolve(pIF, pPort ? pPort : "0", true);
    bool ok = false;

    if (pAddr) {
      ok = bind(hSock, pAddr->ai_addr, (int)pAddr->ai_addrlen) != SOCKET_ERROR;
      if (!ok)
        LOG_WSA_ERROR("bind(\"%s\", \"%s\")", pIF ? pIF : "*", pPort ? pPort : "0");
      freeaddrinfo(pAddr);
    }

    if (!ok) {
      closesocket(hSock);
      return INVALID_SOCKET;
    }
  }
  return hSock;
}

static void SetStreamOptions(SOCKET hSock)
{
  // small writes go out at once instead of waiting for more (Nagle)
  int on = 1;
  if (setsockopt(hSock, IPPROTO_TCP, TCP_NODELAY, (const char *)&on, sizeof(on)) == SOCKET_ERROR)
    LOG_WSA_ERROR("setsockopt(TCP_NODELAY)");

  // a host that is gone (Wi-Fi lost, board reset) is noticed after about 30 s
  struct tcp_keepalive ka;
  DWORD returned;

  ka.onoff = 1;
  ka.keepalivetime = 20000;
  ka.keepaliveinterval = 1000;
  if (WSAIoctl(hSock, SIO_KEEPALIVE_VALS, &ka, sizeof(ka), NULL, 0, &returned, NULL, NULL) == SOCKET_ERROR)
    LOG_WSA_ERROR("WSAIoctl(SIO_KEEPALIVE_VALS)");
}

static void Disconnect(SOCKET hSock)
{
  if (shutdown(hSock, SD_BOTH) == SOCKET_ERROR)
    LOG_WSA_ERROR("Disconnect(): shutdown()");

  if (closesocket(hSock) == SOCKET_ERROR)
    LOG_WSA_ERROR("Disconnect(): closesocket()");

  Log("Disconnect() - OK");
}

// Waits up to timeoutMs (or until the program is stopped). The socket stays
// non-blocking, which does not matter for the overlapped I/O used later.
static bool ConnectWithTimeout(SOCKET hSock, const sockaddr *pAddr, int addrLen, DWORD timeoutMs)
{
  char addr[32];
  WSAEVENT hEvent = WSACreateEvent();

  if (hEvent == WSA_INVALID_EVENT) {
    LOG_WSA_ERROR("WSACreateEvent()");
    return false;
  }

  bool ok = false;
  int err = 0;

  if (WSAEventSelect(hSock, hEvent, FD_CONNECT) == SOCKET_ERROR) {
    LOG_WSA_ERROR("WSAEventSelect()");
  } else if (connect(hSock, pAddr, addrLen) == 0) {
    ok = true;
  } else if ((err = WSAGetLastError()) != WSAEWOULDBLOCK) {
    LogError((DWORD)err, "connect(%s)", AddrStr(pAddr, addr, sizeof(addr)));
  } else {
    HANDLE waits[2] = {hEvent, hStopEvent};
    WSANETWORKEVENTS ne;

    switch (WaitForMultipleObjects(2, waits, FALSE, timeoutMs)) {
      case WAIT_OBJECT_0:
        if (WSAEnumNetworkEvents(hSock, hEvent, &ne) == SOCKET_ERROR)
          LOG_WSA_ERROR("WSAEnumNetworkEvents()");
        else if (ne.iErrorCode[FD_CONNECT_BIT])
          LogError((DWORD)ne.iErrorCode[FD_CONNECT_BIT], "connect(%s)", AddrStr(pAddr, addr, sizeof(addr)));
        else
          ok = true;
        break;
      case WAIT_TIMEOUT:
        Log("connect(%s): no answer in %lu ms", AddrStr(pAddr, addr, sizeof(addr)), (unsigned long)timeoutMs);
        break;
      default:
        break;    // stopped
    }
  }

  WSAEventSelect(hSock, NULL, 0);
  WSACloseEvent(hEvent);
  return ok;
}

// Tries every address the host name has
static SOCKET Connect(const Options &opt)
{
  addrinfo *pList = Resolve(opt.pHost, opt.pPort, false);

  if (!pList)
    return INVALID_SOCKET;

  SOCKET hSock = INVALID_SOCKET;
  char addr[32] = "";

  for (addrinfo *p = pList ; p && hSock == INVALID_SOCKET && !StopRequested() ; p = p->ai_next) {
    hSock = NewSocket(opt.pIF, NULL);
    if (hSock == INVALID_SOCKET)
      break;

    if (ConnectWithTimeout(hSock, p->ai_addr, (int)p->ai_addrlen, 3000)) {
      AddrStr(p->ai_addr, addr, sizeof(addr));
    } else {
      closesocket(hSock);
      hSock = INVALID_SOCKET;
    }
  }
  freeaddrinfo(pList);

  if (hSock == INVALID_SOCKET)
    return INVALID_SOCKET;

  Log("Connect(\"%s\", \"%s\") - OK (%s)", opt.pHost, opt.pPort, addr);
  SetStreamOptions(hSock);
  return hSock;
}

static SOCKET Listen(const Options &opt)
{
  SOCKET hSock = NewSocket(opt.pIF, opt.pPort);

  if (hSock == INVALID_SOCKET)
    return INVALID_SOCKET;

  if (listen(hSock, SOMAXCONN) == SOCKET_ERROR) {
    LOG_WSA_ERROR("listen(\"%s\", \"%s\")", opt.pIF ? opt.pIF : "*", opt.pPort);
    closesocket(hSock);
    return INVALID_SOCKET;
  }
  return hSock;
}

// The next connection. With wait, waits for one (or until the program is
// stopped); without, INVALID_SOCKET when there is none.
static SOCKET Accept(SOCKET hListen, WSAEVENT hAcceptEvent, bool wait)
{
  for (;;) {
    sockaddr_in sn;
    int len = sizeof(sn);
    SOCKET hSock = accept(hListen, (sockaddr *)&sn, &len);

    if (hSock != INVALID_SOCKET) {
      char addr[32];
      Log("Accept(%s) - OK", AddrStr((const sockaddr *)&sn, addr, sizeof(addr)));
      return hSock;
    }

    int err = WSAGetLastError();

    if (err == WSAECONNRESET)
      continue;     // gone again before we took it
    if (err != WSAEWOULDBLOCK) {
      LogError((DWORD)err, "accept()");
      return INVALID_SOCKET;
    }
    if (!wait)
      return INVALID_SOCKET;

    HANDLE waits[2] = {hAcceptEvent, hStopEvent};

    if (WaitForMultipleObjects(2, waits, FALSE, INFINITE) != WAIT_OBJECT_0)
      return INVALID_SOCKET;

    WSANETWORKEVENTS ne;
    WSAEnumNetworkEvents(hListen, hAcceptEvent, &ne);   // resets the event
  }
}

///////////////////////////////////////////////////////////////
// One connection: moves data both ways between the COM port and the socket
// until one side ends.
//
//   COM --read--> protocol.Send() ... protocol.Read() --write--> socket
//   COM <--write-- protocol.Recv() ... protocol.Write() <--read-- socket
//
// Each direction keeps one read and one write under way at the same time.
// A side is not read while the protocol still holds unsent data for the
// other one, so a slow side slows the other down instead of filling memory.

class Session
{
  public:
    Session(HANDLE hCom, SOCKET hSock, Protocol &protocol, const Options &opt,
            SOCKET hListen = INVALID_SOCKET, WSAEVENT hAcceptEvent = WSA_INVALID_EVENT);
    ~Session();

    void Run();

  private:
    enum { evComRead, evSockSend, evSockRecv, evComWrite, evComStat, evSockClose, evAccept, evStop, evNum };

    bool Setup();
    void StartInsertion();
    bool Kick();
    bool Handle(int ev);
    bool CheckDsr();
    bool DsrOptional();
    void FromCom(const BYTE *p, int len);
    void RejectNewcomers();
    void DrainWrites(DWORD ms);
    void Teardown();

    HANDLE hCom;
    SOCKET hSock;
    Protocol &protocol;
    const Options &opt;
    SOCKET hListen;
    WSAEVENT hAcceptEvent;
    WSAEVENT hCloseEvent;

    // COM -> socket
    AsyncOp comRead;
    BYTE_vector comBuf;
    AsyncOp sockSend;
    BYTE_vector sendBuf;
    int sendLen;
    int sendDone;

    // socket -> COM
    AsyncOp sockRecv;
    BYTE_vector recvBuf;
    AsyncOp comWrite;
    BYTE_vector writeBuf;
    int writeLen;
    int writeDone;

    bool closing;         // the host has closed (or is closing) the connection
    bool peerEof;         // ... and everything it sent has been read
    DWORD closingSince;

    AsyncOp comStat;
    DWORD statMask;
    bool watchDsr;
    int dsr;

    bool translate;       // --pusr and the driver inserts
    PusrTranslator pusr;
};

Session::Session(HANDLE _hCom, SOCKET _hSock, Protocol &_protocol, const Options &_opt,
                 SOCKET _hListen, WSAEVENT _hAcceptEvent)
  : hCom(_hCom), hSock(_hSock), protocol(_protocol), opt(_opt),
    hListen(_hListen), hAcceptEvent(_hAcceptEvent), hCloseEvent(WSA_INVALID_EVENT),
    comBuf(kChunk), sendBuf(kChunk), sendLen(0), sendDone(0),
    recvBuf(kChunk), writeBuf(kChunk), writeLen(0), writeDone(0),
    closing(false), peerEof(false), closingSince(0),
    statMask(0), watchDsr(true), dsr(-1), translate(false)
{
}

Session::~Session()
{
  if (hCloseEvent != WSA_INVALID_EVENT)
    WSACloseEvent(hCloseEvent);
}

void Session::Run()
{
  Log("Session() START");

  if (Setup()) {
    while (Kick()) {
      // only what is under way can complete; the rest of the events stay reset
      HANDLE waits[evNum];
      int ids[evNum];
      int n = 0;

      if (comRead.Pending())  { waits[n] = comRead.Event();  ids[n++] = evComRead; }
      if (sockSend.Pending()) { waits[n] = sockSend.Event(); ids[n++] = evSockSend; }
      if (sockRecv.Pending()) { waits[n] = sockRecv.Event(); ids[n++] = evSockRecv; }
      if (comWrite.Pending()) { waits[n] = comWrite.Event(); ids[n++] = evComWrite; }
      if (comStat.Pending())  { waits[n] = comStat.Event();  ids[n++] = evComStat; }
      waits[n] = hCloseEvent;                                ids[n++] = evSockClose;
      if (hAcceptEvent != WSA_INVALID_EVENT) { waits[n] = hAcceptEvent; ids[n++] = evAccept; }
      waits[n] = hStopEvent;                                 ids[n++] = evStop;

      DWORD timeout = INFINITE;
      if (closing) {
        DWORD spent = GetTickCount() - closingSince;
        timeout = spent < kLingerMs ? kLingerMs - spent : 0;
      }

      DWORD r = WaitForMultipleObjects((DWORD)n, waits, FALSE, timeout);

      if (r == WAIT_TIMEOUT)
        continue;
      if (r >= WAIT_OBJECT_0 + (DWORD)n) {
        LOG_LAST_ERROR("WaitForMultipleObjects()");
        break;
      }

      // Handle everything that is ready, not just the first one, so that one
      // direction cannot starve the other. Each further look is one call
      // that does not wait.
      bool ok = true;
      int i = (int)(r - WAIT_OBJECT_0);

      for (;;) {
        ok = Handle(ids[i]);
        if (!ok || ++i >= n)
          break;

        r = WaitForMultipleObjects((DWORD)(n - i), waits + i, FALSE, 0);
        if (r >= WAIT_OBJECT_0 + (DWORD)(n - i))
          break;
        i += (int)(r - WAIT_OBJECT_0);
      }
      if (!ok)
        break;
    }
  }

  Teardown();
  Log("Session() STOP");
}

bool Session::Setup()
{
  protocol.Clean();

  hCloseEvent = WSACreateEvent();
  if (hCloseEvent == WSA_INVALID_EVENT) {
    LOG_WSA_ERROR("WSACreateEvent()");
    return false;
  }

  if (!comRead.Init(hCom) || !sockSend.Init((HANDLE)hSock) ||
      !sockRecv.Init((HANDLE)hSock) || !comWrite.Init(hCom) || !comStat.Init(hCom))
    return false;

  if (WSAEventSelect(hSock, hCloseEvent, FD_CLOSE) == SOCKET_ERROR) {
    LOG_WSA_ERROR("WSAEventSelect()");
    return false;
  }

  if (!SetCommMask(hCom, EV_DSR)) {
    LOG_LAST_ERROR("SetCommMask()");
    if (!DsrOptional())
      return false;
  }

  if (opt.insertion != insNone)
    StartInsertion();

  return true;
}

void Session::StartInsertion()
{
  BYTE_vector current;

  if (!EnableLsrMst(hCom, current)) {
    Log("LSRMST insertion is not available, the settings are not passed on");
    return;
  }

  translate = (opt.insertion == insPusr);

  // the current settings go first
  if (!current.empty())
    FromCom(&current[0], (int)current.size());
}

void Session::FromCom(const BYTE *p, int len)
{
  if (!translate) {
    protocol.Send(p, len);
    return;
  }

  BYTE_vector out;

  out.reserve(len + 16);
  pusr.Translate(p, len, out);
  if (!out.empty())
    protocol.Send(&out[0], (int)out.size());
}

// Starts what is idle and has something to do; false ends the session
bool Session::Kick()
{
  DWORD unused;

  // COM -> socket
  if (!sockSend.Pending()) {
    if (sendDone >= sendLen) {
      int n = protocol.Read(&sendBuf[0], kChunk);
      if (n < 0)
        return false;
      sendLen = n;
      sendDone = 0;
    }
    if (sendDone < sendLen &&
        !sockSend.Started(WriteFile((HANDLE)hSock, &sendBuf[sendDone], (DWORD)(sendLen - sendDone),
                                    &unused, sockSend.Ov()), "WriteFile(socket)"))
      return false;
  }

  if (!comRead.Pending() && !protocol.isSendFull() &&
      !comRead.Started(ReadFile(hCom, &comBuf[0], kChunk, &unused, comRead.Ov()), "ReadFile(COM)"))
    return false;

  // socket -> COM
  if (!comWrite.Pending()) {
    if (writeDone >= writeLen) {
      int n = protocol.Recv(&writeBuf[0], kChunk);
      if (n < 0)
        return false;
      writeLen = n;
      writeDone = 0;
    }
    if (writeDone < writeLen &&
        !comWrite.Started(WriteFile(hCom, &writeBuf[writeDone], (DWORD)(writeLen - writeDone),
                                    &unused, comWrite.Ov()), "WriteFile(COM)"))
      return false;
  }

  if (!peerEof && !sockRecv.Pending() && !protocol.isWriteFull() &&
      !sockRecv.Started(ReadFile((HANDLE)hSock, &recvBuf[0], kChunk, &unused, sockRecv.Ov()), "ReadFile(socket)"))
    return false;

  // The host has closed: write out what it sent, but do not wait forever for
  // a paired port nobody reads
  if (closing) {
    if (peerEof && !comWrite.Pending()) {
      Log("All data from the host has been written");
      return false;
    }
    if (GetTickCount() - closingSince >= kLingerMs) {
      Log("Gave up writing the rest of the data from the host");
      return false;
    }
  }

  // DSR (the paired port's DTR)
  if (watchDsr && !comStat.Pending()) {
    if (!comStat.Started(WaitCommEvent(hCom, &statMask, comStat.Ov()), "WaitCommEvent()"))
      return DsrOptional();
    if (!CheckDsr())
      return false;
  }

  return true;
}

bool Session::Handle(int ev)
{
  DWORD n;

  switch (ev) {
    case evComRead:
      if (comRead.Finish(n, "ReadFile(COM)") == AsyncOp::failed)
        return false;
      FromCom(&comBuf[0], (int)n);
      return true;

    case evSockSend:
      if (sockSend.Finish(n, "WriteFile(socket)") == AsyncOp::failed)
        return false;
      sendDone += (int)n;
      return true;

    case evSockRecv: {
      AsyncOp::Result r = sockRecv.Finish(n, "ReadFile(socket)");

      if (r == AsyncOp::failed)
        return false;
      if (r == AsyncOp::done && n == 0) {
        Log("Received EOF");
        peerEof = true;
        if (!closing) {
          closing = true;
          closingSince = GetTickCount();
        }
        return true;
      }
      protocol.Write(&recvBuf[0], (int)n);
      return true;
    }

    case evComWrite:
      if (comWrite.Finish(n, "WriteFile(COM)") == AsyncOp::failed)
        return false;
      writeDone += (int)n;
      return true;

    case evComStat:
      if (comStat.Finish(n, "WaitCommEvent()") == AsyncOp::failed)
        return DsrOptional();
      return true;    // the next Kick() looks at DSR and waits again

    case evSockClose: {
      WSANETWORKEVENTS ne;

      memset(&ne, 0, sizeof(ne));
      WSAEnumNetworkEvents(hSock, hCloseEvent, &ne);
      WSAResetEvent(hCloseEvent);

      int err = ne.iErrorCode[FD_CLOSE_BIT];
      if (err) {
        // reset or timed out: there is nothing more to read
        LogError((DWORD)err, "Connection lost");
        return false;
      }

      // closed in order: what is still in the socket is read up to EOF
      Log("EVENT_CLOSE");
      if (!closing) {
        closing = true;
        closingSince = GetTickCount();
      }
      return true;
    }

    case evAccept:
      RejectNewcomers();
      return true;

    case evStop:
      Log("Stop requested");
      return false;
  }
  return true;
}

bool Session::CheckDsr()
{
  DWORD stat;

  if (!GetCommModemStatus(hCom, &stat)) {
    LOG_LAST_ERROR("GetCommModemStatus()");
    return DsrOptional();
  }

  int on = (stat & MS_DSR_ON) ? 1 : 0;

  if (on != dsr) {
    Log("DSR is %s", on ? "ON" : "OFF");
    dsr = on;
  }

  // the program on the paired port has gone
  if (!on && !opt.com.IgnoreDSR())
    return false;

  return true;
}

// With --ignore-dsr DSR is only shown, so a port that cannot report it is fine
bool Session::DsrOptional()
{
  if (!opt.com.IgnoreDSR())
    return false;

  watchDsr = false;
  return true;
}

// Server mode: the COM port is taken, so turn away whoever else connects
void Session::RejectNewcomers()
{
  WSANETWORKEVENTS ne;
  WSAEnumNetworkEvents(hListen, hAcceptEvent, &ne);   // resets the event

  Log("EVENT_ACCEPT");

  for (;;) {
    SOCKET hTmp = Accept(hListen, hAcceptEvent, false);

    if (hTmp == INVALID_SOCKET)
      break;

    static const char msg[] = "*** Serial port is busy ***\n";
    send(hTmp, msg, (int)strlen(msg), 0);
    Disconnect(hTmp);
  }
}

// Gives the writes that are under way up to ms to finish
void Session::DrainWrites(DWORD ms)
{
  DWORD start = GetTickCount();

  while (sockSend.Pending() || comWrite.Pending()) {
    DWORD spent = GetTickCount() - start;
    if (spent >= ms)
      break;

    HANDLE waits[2];
    AsyncOp *ops[2];
    const char *names[2];
    int n = 0;

    if (sockSend.Pending()) {
      waits[n] = sockSend.Event();
      ops[n] = &sockSend;
      names[n++] = "WriteFile(socket)";
    }
    if (comWrite.Pending()) {
      waits[n] = comWrite.Event();
      ops[n] = &comWrite;
      names[n++] = "WriteFile(COM)";
    }

    DWORD r = WaitForMultipleObjects((DWORD)n, waits, FALSE, ms - spent);
    if (r >= WAIT_OBJECT_0 + (DWORD)n)
      break;

    DWORD bytes;
    ops[r - WAIT_OBJECT_0]->Finish(bytes, names[r - WAIT_OBJECT_0]);
  }
}

void Session::Teardown()
{
  DrainWrites(StopRequested() ? 0 : kLingerMs);

  // CancelIo() cancels what this thread started on the handle; the
  // operations still have to be waited for before their buffers go away
  CancelIo(hCom);
  CancelIo((HANDLE)hSock);

  comRead.WaitCancelled();
  sockSend.WaitCancelled();
  sockRecv.WaitCancelled();
  comWrite.WaitCancelled();
  comStat.WaitCancelled();

  if (hCloseEvent != WSA_INVALID_EVENT)
    WSAEventSelect(hSock, NULL, 0);
}

///////////////////////////////////////////////////////////////
// Client mode: before connecting, wait until the COM side is there
//   --awak-seq     until the sequence has been read (it and everything
//                  before it is dropped)
//   otherwise      until DSR is ON and still ON a second later
//                  (unless --ignore-dsr)

static bool WaitAwakSeq(HANDLE hCom, const BYTE *pSeq)
{
  AsyncOp rd;

  if (!rd.Init(hCom))
    return false;

  const BYTE *pNext = pSeq;
  BYTE ch = 0;
  bool ok = false;

  while (!StopRequested()) {
    if (!*pNext) {
      ok = true;
      break;
    }

    DWORD unused;
    if (!rd.Started(ReadFile(hCom, &ch, 1, &unused, rd.Ov()), "WaitComReady(): ReadFile()"))
      break;

    HANDLE waits[2] = {rd.Event(), hStopEvent};
    if (WaitForMultipleObjects(2, waits, FALSE, INFINITE) != WAIT_OBJECT_0)
      break;

    DWORD n;
    if (rd.Finish(n, "WaitComReady(): ReadFile()") == AsyncOp::failed)
      break;
    if (!n)
      continue;

    if (*pNext == ch)
      pNext++;
    else
      pNext = (*pSeq == ch) ? pSeq + 1 : pSeq;

    Log("Skipped character 0x%02X", (unsigned)ch);
  }

  CancelIo(hCom);
  rd.WaitCancelled();
  return ok;
}

static bool WaitDsrOn(HANDLE hCom)
{
  AsyncOp st;
  DWORD mask = 0;

  if (!st.Init(hCom))
    return false;

  if (!SetCommMask(hCom, EV_DSR)) {
    LOG_LAST_ERROR("WaitComReady(): SetCommMask()");
    return false;
  }

  bool ok = false;

  while (!StopRequested()) {
    if (!st.Started(WaitCommEvent(hCom, &mask, st.Ov()), "WaitComReady(): WaitCommEvent()"))
      break;

    DWORD stat;
    if (!GetCommModemStatus(hCom, &stat)) {
      LOG_LAST_ERROR("WaitComReady(): GetCommModemStatus()");
      break;
    }

    if (stat & MS_DSR_ON) {
      Log("DSR is ON");

      if (!Pause(1000))
        break;

      if (!GetCommModemStatus(hCom, &stat)) {
        LOG_LAST_ERROR("WaitComReady(): GetCommModemStatus()");
        break;
      }
      if (stat & MS_DSR_ON) {
        ok = true;
        break;
      }
      Log("DSR is OFF");
    }

    HANDLE waits[2] = {st.Event(), hStopEvent};
    if (WaitForMultipleObjects(2, waits, FALSE, INFINITE) != WAIT_OBJECT_0)
      break;

    DWORD n;
    if (st.Finish(n, "WaitComReady(): WaitCommEvent()") == AsyncOp::failed)
      break;
  }

  CancelIo(hCom);
  st.WaitCancelled();
  return ok;
}

static bool WaitComReady(HANDLE hCom, const Options &opt)
{
  bool ok;

  if (opt.pAwakSeq && *opt.pAwakSeq)
    ok = WaitAwakSeq(hCom, opt.pAwakSeq);
  else if (!opt.com.IgnoreDSR())
    ok = WaitDsrOn(hCom);
  else
    return !StopRequested();

  Log("WaitComReady() - %s", ok ? "OK" : "FAIL");
  return ok && !StopRequested();
}

///////////////////////////////////////////////////////////////
// Client mode: the COM port stays open; connect whenever the COM side is ready
static int RunClient(const Options &opt, Protocol &protocol)
{
  HANDLE hCom = OpenComPort(opt);

  if (hCom == INVALID_HANDLE_VALUE)
    return 2;

  while (WaitComReady(hCom, opt)) {
    SOCKET hSock = Connect(opt);

    if (hSock == INVALID_SOCKET)
      break;

    DWORD start = GetTickCount();

    Session(hCom, hSock, protocol, opt).Run();
    Disconnect(hSock);

    // a session that ends at once would otherwise reconnect in a tight loop
    if (GetTickCount() - start < 1000 && !Pause(1000))
      break;
  }

  CloseHandle(hCom);
  return 2;
}

// Server mode: open the COM port for each connection
static int RunServer(const Options &opt, Protocol &protocol)
{
  SOCKET hListen = Listen(opt);

  if (hListen == INVALID_SOCKET)
    return 2;

  WSAEVENT hAcceptEvent = WSACreateEvent();

  if (hAcceptEvent == WSA_INVALID_EVENT || WSAEventSelect(hListen, hAcceptEvent, FD_ACCEPT) == SOCKET_ERROR) {
    LOG_WSA_ERROR("WSAEventSelect(FD_ACCEPT)");
  } else {
    for (;;) {
      SOCKET hSock = Accept(hListen, hAcceptEvent, true);

      if (hSock == INVALID_SOCKET)
        break;

      SetStreamOptions(hSock);

      HANDLE hCom = OpenComPort(opt);

      if (hCom != INVALID_HANDLE_VALUE) {
        Session(hCom, hSock, protocol, opt, hListen, hAcceptEvent).Run();
        CloseHandle(hCom);
      }

      Disconnect(hSock);
    }
  }

  if (hAcceptEvent != WSA_INVALID_EVENT)
    WSACloseEvent(hAcceptEvent);
  closesocket(hListen);
  return 2;
}

///////////////////////////////////////////////////////////////
int main(int argc, char *argv[])
{
  Options opt;

  if (!ParseArgs(argc, argv, opt))
    Usage(argv[0]);

  hStopEvent = CreateEvent(NULL, TRUE, FALSE, NULL);
  if (!hStopEvent) {
    LOG_LAST_ERROR("CreateEvent()");
    return 2;
  }
  SetConsoleCtrlHandler(OnConsoleCtrl, TRUE);

  WSADATA wsaData;
  int err = WSAStartup(MAKEWORD(2, 2), &wsaData);

  if (err) {
    LogError((DWORD)err, "WSAStartup()");
    return 2;
  }

  Log("com2tcp %s", C2T_VERSION_STR);
  if (opt.insertion == insLsrMst)
    Log("LsrMstInsert enabled");
  else if (opt.insertion == insPusr)
    Log("LsrMstInsert & PUSR's packet support enabled");

  // both live here, so nothing is deleted through the base class (Protocol
  // has no virtual destructor)
  Protocol raw(10, 10);
  TelnetProtocol telnet(10, 10);

  telnet.SetTerminalType(opt.pTermType);

  Protocol &protocol = opt.telnet ? static_cast<Protocol &>(telnet) : raw;

  int res = opt.pHost ? RunClient(opt, protocol) : RunServer(opt, protocol);

  WSACleanup();
  CloseHandle(hStopEvent);
  return res;
}
///////////////////////////////////////////////////////////////
