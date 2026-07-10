/* Minimal <windows.h> shim for c99mtlc.
 * Declarations only; kernel32/ucrtbase/msvcrt exports auto-import via the
 * libmtlc PE linker. Win64 has a single calling convention, so no
 * __stdcall annotations are needed. */
#ifndef _WINDOWS_H
#define _WINDOWS_H

#include <stddef.h>

/* ---- base types ---- */
typedef unsigned char BYTE, UCHAR;
typedef unsigned short WORD, USHORT, WCHAR;
typedef unsigned int DWORD, UINT, ULONG, ULONG32;
typedef int BOOL, INT, LONG, LONG32;
typedef long long LONGLONG, LONG64, INT_PTR, LONG_PTR;
typedef unsigned long long ULONGLONG, ULONG64, DWORD64, UINT_PTR, ULONG_PTR,
    SIZE_T, DWORD_PTR;
typedef long long SSIZE_T;
typedef void *PVOID, *LPVOID, *HANDLE, *HMODULE, *HINSTANCE, *HLOCAL,
    *HGLOBAL;
typedef const void *LPCVOID;
typedef char CHAR, TCHAR, *PSTR, *LPSTR, *PCHAR;
typedef const char *LPCSTR, *PCSTR;
typedef WCHAR *LPWSTR, *PWSTR;
typedef const WCHAR *LPCWSTR, *PCWSTR;
typedef BYTE *PBYTE, *LPBYTE;
typedef DWORD *PDWORD, *LPDWORD;
typedef BOOL *PBOOL, *LPBOOL;
typedef void (*FARPROC)(void);

#define VOID void
#define WINAPI
#define APIENTRY
#define CALLBACK
#define CONST const
#define TRUE 1
#define FALSE 0
#define MAX_PATH 260
#define INFINITE 0xFFFFFFFFu
#define INVALID_HANDLE_VALUE ((HANDLE)(LONG_PTR)-1)
#define NULL ((void *)0)

typedef union _LARGE_INTEGER {
  struct {
    DWORD LowPart;
    LONG HighPart;
  };
  LONGLONG QuadPart;
} LARGE_INTEGER, *PLARGE_INTEGER;

typedef union _ULARGE_INTEGER {
  struct {
    DWORD LowPart;
    DWORD HighPart;
  };
  ULONGLONG QuadPart;
} ULARGE_INTEGER;

typedef struct _FILETIME {
  DWORD dwLowDateTime;
  DWORD dwHighDateTime;
} FILETIME, *PFILETIME;

typedef struct _SYSTEMTIME {
  WORD wYear, wMonth, wDayOfWeek, wDay, wHour, wMinute, wSecond,
      wMilliseconds;
} SYSTEMTIME;

typedef struct _SECURITY_ATTRIBUTES {
  DWORD nLength;
  LPVOID lpSecurityDescriptor;
  BOOL bInheritHandle;
} SECURITY_ATTRIBUTES, *LPSECURITY_ATTRIBUTES;

/* ---- errors / last-error ---- */
DWORD GetLastError(void);
void SetLastError(DWORD err);
#define ERROR_SUCCESS 0
#define ERROR_FILE_NOT_FOUND 2
#define ERROR_ACCESS_DENIED 5
#define ERROR_ALREADY_EXISTS 183

/* ---- console / std handles ---- */
#define STD_INPUT_HANDLE ((DWORD)-10)
#define STD_OUTPUT_HANDLE ((DWORD)-11)
#define STD_ERROR_HANDLE ((DWORD)-12)
#define ENABLE_PROCESSED_OUTPUT 0x0001
#define ENABLE_WRAP_AT_EOL_OUTPUT 0x0002
#define ENABLE_VIRTUAL_TERMINAL_PROCESSING 0x0004
HANDLE GetStdHandle(DWORD which);
BOOL GetConsoleMode(HANDLE h, DWORD *mode);
BOOL SetConsoleMode(HANDLE h, DWORD mode);
UINT GetConsoleOutputCP(void);
BOOL SetConsoleOutputCP(UINT cp);
#define CP_ACP 0
#define CP_UTF8 65001
BOOL WriteConsoleA(HANDLE h, const void *buf, DWORD n, DWORD *written,
                   void *reserved);

/* ---- file I/O ---- */
#define GENERIC_READ 0x80000000u
#define GENERIC_WRITE 0x40000000u
#define FILE_SHARE_READ 0x00000001u
#define FILE_SHARE_WRITE 0x00000002u
#define CREATE_NEW 1
#define CREATE_ALWAYS 2
#define OPEN_EXISTING 3
#define OPEN_ALWAYS 4
#define TRUNCATE_EXISTING 5
#define FILE_ATTRIBUTE_NORMAL 0x00000080u
#define FILE_ATTRIBUTE_DIRECTORY 0x00000010u
#define INVALID_FILE_ATTRIBUTES ((DWORD)-1)
#define FILE_BEGIN 0
#define FILE_CURRENT 1
#define FILE_END 2
HANDLE CreateFileA(LPCSTR path, DWORD access, DWORD share,
                   LPSECURITY_ATTRIBUTES sa, DWORD disp, DWORD flags,
                   HANDLE template_file);
BOOL ReadFile(HANDLE h, void *buf, DWORD n, DWORD *read, void *ovl);
BOOL PeekNamedPipe(HANDLE h, LPVOID buf, DWORD n, LPDWORD read,
                   LPDWORD avail, LPDWORD left);
BOOL WriteFile(HANDLE h, const void *buf, DWORD n, DWORD *written, void *ovl);
BOOL CloseHandle(HANDLE h);
BOOL FlushFileBuffers(HANDLE h);
DWORD SetFilePointer(HANDLE h, LONG lo, LONG *hi, DWORD method);
BOOL GetFileSizeEx(HANDLE h, LARGE_INTEGER *size);
DWORD GetFileAttributesA(LPCSTR path);
BOOL DeleteFileA(LPCSTR path);
BOOL MoveFileA(LPCSTR from, LPCSTR to);
BOOL CopyFileA(LPCSTR from, LPCSTR to, BOOL fail_if_exists);
BOOL CreateDirectoryA(LPCSTR path, LPSECURITY_ATTRIBUTES sa);
BOOL RemoveDirectoryA(LPCSTR path);
DWORD GetCurrentDirectoryA(DWORD n, LPSTR buf);
BOOL SetCurrentDirectoryA(LPCSTR path);
DWORD GetTempPathA(DWORD n, LPSTR buf);
DWORD GetFullPathNameA(LPCSTR path, DWORD n, LPSTR buf, LPSTR *file_part);

/* ---- modules / processes / env ---- */
HMODULE LoadLibraryA(LPCSTR name);
FARPROC GetProcAddress(HMODULE mod, LPCSTR name);
BOOL FreeLibrary(HMODULE mod);
HMODULE GetModuleHandleA(LPCSTR name);
BOOL GetModuleHandleExA(DWORD flags, LPCSTR name, HMODULE *out);
DWORD GetModuleFileNameA(HMODULE mod, LPSTR buf, DWORD n);
#define GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS 0x00000004
#define GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT 0x00000002
DWORD GetEnvironmentVariableA(LPCSTR name, LPSTR buf, DWORD n);
BOOL SetEnvironmentVariableA(LPCSTR name, LPCSTR value);
void ExitProcess(UINT code);
BOOL TerminateProcess(HANDLE h, UINT code);
HANDLE GetCurrentProcess(void);
HANDLE GetCurrentThread(void);
DWORD GetCurrentProcessId(void);
DWORD GetCurrentThreadId(void);
BOOL IsDebuggerPresent(void);
void DebugBreak(void);
void OutputDebugStringA(LPCSTR s);
LPSTR GetCommandLineA(void);

/* ---- timing ---- */
BOOL QueryPerformanceCounter(LARGE_INTEGER *out);
BOOL QueryPerformanceFrequency(LARGE_INTEGER *out);
DWORD GetTickCount(void);
ULONGLONG GetTickCount64(void);
void Sleep(DWORD ms);
void GetSystemTimeAsFileTime(FILETIME *out);
void GetLocalTime(SYSTEMTIME *out);
void GetSystemTime(SYSTEMTIME *out);

/* ---- interlocked ----
 * On x64 these are compiler intrinsics, not kernel32 exports, so the PE
 * import prober cannot resolve them. Plain load/store stand-ins: not atomic,
 * fine for the compiler driver's one-shot guards and refcounts. */
static LONG InterlockedIncrement(LONG volatile *p) { return ++(*p); }
static LONG InterlockedDecrement(LONG volatile *p) { return --(*p); }
static LONG InterlockedExchange(LONG volatile *p, LONG v) {
  LONG old = *p;
  *p = v;
  return old;
}
static LONG InterlockedExchangeAdd(LONG volatile *p, LONG v) {
  LONG old = *p;
  *p = old + v;
  return old;
}
static LONG InterlockedCompareExchange(LONG volatile *p, LONG nv, LONG cmp) {
  LONG old = *p;
  if (old == cmp)
    *p = nv;
  return old;
}
static LONGLONG InterlockedCompareExchange64(LONGLONG volatile *p, LONGLONG nv,
                                             LONGLONG cmp) {
  LONGLONG old = *p;
  if (old == cmp)
    *p = nv;
  return old;
}

/* ---- synchronization ---- */
typedef struct _CRITICAL_SECTION {
  void *DebugInfo;
  LONG LockCount;
  LONG RecursionCount;
  HANDLE OwningThread;
  HANDLE LockSemaphore;
  ULONG_PTR SpinCount;
} CRITICAL_SECTION, *LPCRITICAL_SECTION;

typedef struct _CONDITION_VARIABLE {
  PVOID Ptr;
} CONDITION_VARIABLE, *PCONDITION_VARIABLE;

typedef struct _SRWLOCK {
  PVOID Ptr;
} SRWLOCK, *PSRWLOCK;

void InitializeCriticalSection(LPCRITICAL_SECTION cs);
BOOL InitializeCriticalSectionAndSpinCount(LPCRITICAL_SECTION cs, DWORD spin);
void EnterCriticalSection(LPCRITICAL_SECTION cs);
BOOL TryEnterCriticalSection(LPCRITICAL_SECTION cs);
void LeaveCriticalSection(LPCRITICAL_SECTION cs);
void DeleteCriticalSection(LPCRITICAL_SECTION cs);
void InitializeConditionVariable(PCONDITION_VARIABLE cv);
BOOL SleepConditionVariableCS(PCONDITION_VARIABLE cv, LPCRITICAL_SECTION cs,
                              DWORD ms);
void WakeConditionVariable(PCONDITION_VARIABLE cv);
void WakeAllConditionVariable(PCONDITION_VARIABLE cv);

typedef DWORD (*LPTHREAD_START_ROUTINE)(LPVOID arg);
HANDLE CreateThread(LPSECURITY_ATTRIBUTES sa, SIZE_T stack,
                    LPTHREAD_START_ROUTINE start, LPVOID arg, DWORD flags,
                    DWORD *tid);
DWORD WaitForSingleObject(HANDLE h, DWORD ms);
#define WAIT_OBJECT_0 0
#define WAIT_TIMEOUT 0x102
#define WAIT_FAILED 0xFFFFFFFFu
HANDLE CreateMutexA(LPSECURITY_ATTRIBUTES sa, BOOL initial_owner,
                    LPCSTR name);
BOOL ReleaseMutex(HANDLE h);
HANDLE CreateEventA(LPSECURITY_ATTRIBUTES sa, BOOL manual, BOOL initial,
                    LPCSTR name);
BOOL SetEvent(HANDLE h);
BOOL ResetEvent(HANDLE h);

/* ---- TLS / FLS ---- */
#define TLS_OUT_OF_INDEXES 0xFFFFFFFFu
#define FLS_OUT_OF_INDEXES 0xFFFFFFFFu
DWORD TlsAlloc(void);
BOOL TlsFree(DWORD idx);
LPVOID TlsGetValue(DWORD idx);
BOOL TlsSetValue(DWORD idx, LPVOID v);
typedef void (*PFLS_CALLBACK_FUNCTION)(PVOID data);
DWORD FlsAlloc(PFLS_CALLBACK_FUNCTION cb);
BOOL FlsFree(DWORD idx);
PVOID FlsGetValue(DWORD idx);
BOOL FlsSetValue(DWORD idx, PVOID v);

/* ---- virtual memory ---- */
#define MEM_COMMIT 0x1000
#define MEM_RESERVE 0x2000
#define MEM_RELEASE 0x8000
#define MEM_FREE 0x10000
#define PAGE_NOACCESS 0x01
#define PAGE_READONLY 0x02
#define PAGE_READWRITE 0x04
#define PAGE_EXECUTE 0x10
#define PAGE_EXECUTE_READ 0x20
#define PAGE_EXECUTE_READWRITE 0x40
#define PAGE_GUARD 0x100
typedef struct _MEMORY_BASIC_INFORMATION {
  PVOID BaseAddress;
  PVOID AllocationBase;
  DWORD AllocationProtect;
  WORD PartitionId;
  SIZE_T RegionSize;
  DWORD State;
  DWORD Protect;
  DWORD Type;
} MEMORY_BASIC_INFORMATION, *PMEMORY_BASIC_INFORMATION;
LPVOID VirtualAlloc(LPVOID addr, SIZE_T size, DWORD type, DWORD protect);
BOOL VirtualFree(LPVOID addr, SIZE_T size, DWORD type);
BOOL VirtualProtect(LPVOID addr, SIZE_T size, DWORD prot, DWORD *old);
SIZE_T VirtualQuery(LPCVOID addr, MEMORY_BASIC_INFORMATION *info,
                    SIZE_T len);

/* ---- heap ---- */
HANDLE GetProcessHeap(void);
LPVOID HeapAlloc(HANDLE heap, DWORD flags, SIZE_T n);
LPVOID HeapReAlloc(HANDLE heap, DWORD flags, LPVOID p, SIZE_T n);
BOOL HeapFree(HANDLE heap, DWORD flags, LPVOID p);
#define HEAP_ZERO_MEMORY 0x8

/* ---- system info ---- */
typedef struct _SYSTEM_INFO {
  DWORD dwOemId;
  DWORD dwPageSize;
  LPVOID lpMinimumApplicationAddress;
  LPVOID lpMaximumApplicationAddress;
  DWORD_PTR dwActiveProcessorMask;
  DWORD dwNumberOfProcessors;
  DWORD dwProcessorType;
  DWORD dwAllocationGranularity;
  WORD wProcessorLevel;
  WORD wProcessorRevision;
} SYSTEM_INFO, *LPSYSTEM_INFO;
void GetSystemInfo(LPSYSTEM_INFO out);
void GetNativeSystemInfo(LPSYSTEM_INFO out);

/* ---- structured exception handling ---- */
#define EXCEPTION_MAXIMUM_PARAMETERS 15
#define EXCEPTION_EXECUTE_HANDLER 1
#define EXCEPTION_CONTINUE_SEARCH 0
#define EXCEPTION_CONTINUE_EXECUTION (-1)
#define EXCEPTION_ACCESS_VIOLATION 0xC0000005u
#define EXCEPTION_ARRAY_BOUNDS_EXCEEDED 0xC000008Cu
#define EXCEPTION_DATATYPE_MISALIGNMENT 0x80000002u
#define EXCEPTION_FLT_DIVIDE_BY_ZERO 0xC000008Eu
#define EXCEPTION_FLT_INVALID_OPERATION 0xC0000090u
#define EXCEPTION_FLT_OVERFLOW 0xC0000091u
#define EXCEPTION_FLT_UNDERFLOW 0xC0000093u
#define EXCEPTION_ILLEGAL_INSTRUCTION 0xC000001Du
#define EXCEPTION_IN_PAGE_ERROR 0xC0000006u
#define EXCEPTION_INT_DIVIDE_BY_ZERO 0xC0000094u
#define EXCEPTION_INT_OVERFLOW 0xC0000095u
#define EXCEPTION_STACK_OVERFLOW 0xC00000FDu
#define EXCEPTION_BREAKPOINT 0x80000003u
#define EXCEPTION_SINGLE_STEP 0x80000004u
#define EXCEPTION_PRIV_INSTRUCTION 0xC0000096u
#define EXCEPTION_GUARD_PAGE 0x80000001u
#define EXCEPTION_INVALID_HANDLE 0xC0000008u
#define EXCEPTION_NONCONTINUABLE_EXCEPTION 0xC0000025u
#define STATUS_HEAP_CORRUPTION 0xC0000374u
#define STATUS_STACK_BUFFER_OVERRUN 0xC0000409u
#define DBG_PRINTEXCEPTION_C 0x40010006u
#define DBG_PRINTEXCEPTION_WIDE_C 0x4001000Au

typedef struct _M128A {
  ULONGLONG Low;
  LONGLONG High;
} M128A;

/* AMD64 CONTEXT with exact winnt.h offsets for the named registers */
typedef struct _CONTEXT {
  DWORD64 P1Home, P2Home, P3Home, P4Home, P5Home, P6Home;
  DWORD ContextFlags;
  DWORD MxCsr;
  WORD SegCs, SegDs, SegEs, SegFs, SegGs, SegSs;
  DWORD EFlags;
  DWORD64 Dr0, Dr1, Dr2, Dr3, Dr6, Dr7;
  DWORD64 Rax, Rcx, Rdx, Rbx, Rsp, Rbp, Rsi, Rdi;
  DWORD64 R8, R9, R10, R11, R12, R13, R14, R15;
  DWORD64 Rip;
  BYTE FltSave[512];
  M128A VectorRegister[26];
  DWORD64 VectorControl;
  DWORD64 DebugControl;
  DWORD64 LastBranchToRip;
  DWORD64 LastBranchFromRip;
  DWORD64 LastExceptionToRip;
  DWORD64 LastExceptionFromRip;
} CONTEXT, *PCONTEXT;

typedef struct _EXCEPTION_RECORD {
  DWORD ExceptionCode;
  DWORD ExceptionFlags;
  struct _EXCEPTION_RECORD *ExceptionRecord;
  PVOID ExceptionAddress;
  DWORD NumberParameters;
  ULONG_PTR ExceptionInformation[EXCEPTION_MAXIMUM_PARAMETERS];
} EXCEPTION_RECORD, *PEXCEPTION_RECORD;

typedef struct _EXCEPTION_POINTERS {
  PEXCEPTION_RECORD ExceptionRecord;
  PCONTEXT ContextRecord;
} EXCEPTION_POINTERS, *PEXCEPTION_POINTERS, *LPEXCEPTION_POINTERS;

typedef LONG (*PVECTORED_EXCEPTION_HANDLER)(EXCEPTION_POINTERS *info);
typedef LONG (*LPTOP_LEVEL_EXCEPTION_FILTER)(EXCEPTION_POINTERS *info);
typedef LPTOP_LEVEL_EXCEPTION_FILTER PTOP_LEVEL_EXCEPTION_FILTER;

LPTOP_LEVEL_EXCEPTION_FILTER
SetUnhandledExceptionFilter(LPTOP_LEVEL_EXCEPTION_FILTER filter);
PVOID AddVectoredExceptionHandler(ULONG first,
                                  PVECTORED_EXCEPTION_HANDLER handler);
ULONG RemoveVectoredExceptionHandler(PVOID handle);
void RaiseException(DWORD code, DWORD flags, DWORD nargs,
                    const ULONG_PTR *args);
void RtlCaptureContext(PCONTEXT ctx);
USHORT RtlCaptureStackBackTrace(ULONG skip, ULONG count, PVOID *frames,
                                ULONG *hash);
#define CaptureStackBackTrace RtlCaptureStackBackTrace

/* ---- TEB (best-effort; not importable, degrade gracefully) ---- */
typedef struct _NT_TIB {
  PVOID ExceptionList;
  PVOID StackBase;
  PVOID StackLimit;
  PVOID SubSystemTib;
  PVOID FiberData;
  PVOID ArbitraryUserPointer;
  struct _NT_TIB *Self;
} NT_TIB;
/* ntdll is outside the import set; callers must handle NULL */
#define NtCurrentTeb() ((NT_TIB *)0)

#endif /* _WINDOWS_H */
