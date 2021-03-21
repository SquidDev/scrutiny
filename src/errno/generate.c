#include <stdio.h>
#include <errno.h>

#define ERRNO(x) printf("| %d -> Unix." #x "\n", x);

int main() {
    printf("let convert = function\n");
#ifdef E2BI
    ERRNO(E2BI)
#endif
#ifdef EACCES
    ERRNO(EACCES)
#endif
#ifdef EAGAIN
    ERRNO(EAGAIN)
#endif
#ifdef EBADF
    ERRNO(EBADF)
#endif
#ifdef EBUSY
    ERRNO(EBUSY)
#endif
#ifdef ECHILD
    ERRNO(ECHILD)
#endif
#ifdef EDEADLK
    ERRNO(EDEADLK)
#endif
#ifdef EDOM
    ERRNO(EDOM)
#endif
#ifdef EEXIST
    ERRNO(EEXIST)
#endif
#ifdef EFAULT
    ERRNO(EFAULT)
#endif
#ifdef EFBIG
    ERRNO(EFBIG)
#endif
#ifdef EINTR
    ERRNO(EINTR)
#endif
#ifdef EINVAL
    ERRNO(EINVAL)
#endif
#ifdef EIO
    ERRNO(EIO)
#endif
#ifdef EISDIR
    ERRNO(EISDIR)
#endif
#ifdef EMFILE
    ERRNO(EMFILE)
#endif
#ifdef EMLINK
    ERRNO(EMLINK)
#endif
#ifdef ENAMETOOLONG
    ERRNO(ENAMETOOLONG)
#endif
#ifdef ENFILE
    ERRNO(ENFILE)
#endif
#ifdef ENODEV
    ERRNO(ENODEV)
#endif
#ifdef ENOENT
    ERRNO(ENOENT)
#endif
#ifdef ENOEXEC
    ERRNO(ENOEXEC)
#endif
#ifdef ENOLCK
    ERRNO(ENOLCK)
#endif
#ifdef ENOMEM
    ERRNO(ENOMEM)
#endif
#ifdef ENOSPC
    ERRNO(ENOSPC)
#endif
#ifdef ENOSYS
    ERRNO(ENOSYS)
#endif
#ifdef ENOTDIR
    ERRNO(ENOTDIR)
#endif
#ifdef ENOTEMPTY
    ERRNO(ENOTEMPTY)
#endif
#ifdef ENOTTY
    ERRNO(ENOTTY)
#endif
#ifdef ENXIO
    ERRNO(ENXIO)
#endif
#ifdef EPERM
    ERRNO(EPERM)
#endif
#ifdef EPIPE
    ERRNO(EPIPE)
#endif
#ifdef ERANGE
    ERRNO(ERANGE)
#endif
#ifdef EROFS
    ERRNO(EROFS)
#endif
#ifdef ESPIPE
    ERRNO(ESPIPE)
#endif
#ifdef ESRCH
    ERRNO(ESRCH)
#endif
#ifdef EXDEV
    ERRNO(EXDEV)
#endif
#ifdef EWOULDBLOCK
    ERRNO(EWOULDBLOCK)
#endif
#ifdef EINPROGRESS
    ERRNO(EINPROGRESS)
#endif
#ifdef EALREADY
    ERRNO(EALREADY)
#endif
#ifdef ENOTSOCK
    ERRNO(ENOTSOCK)
#endif
#ifdef EDESTADDRREQ
    ERRNO(EDESTADDRREQ)
#endif
#ifdef EMSGSIZE
    ERRNO(EMSGSIZE)
#endif
#ifdef EPROTOTYPE
    ERRNO(EPROTOTYPE)
#endif
#ifdef ENOPROTOOPT
    ERRNO(ENOPROTOOPT)
#endif
#ifdef EPROTONOSUPPORT
    ERRNO(EPROTONOSUPPORT)
#endif
#ifdef ESOCKTNOSUPPORT
    ERRNO(ESOCKTNOSUPPORT)
#endif
#ifdef EOPNOTSUPP
    ERRNO(EOPNOTSUPP)
#endif
#ifdef EPFNOSUPPORT
    ERRNO(EPFNOSUPPORT)
#endif
#ifdef EAFNOSUPPORT
    ERRNO(EAFNOSUPPORT)
#endif
#ifdef EADDRINUSE
    ERRNO(EADDRINUSE)
#endif
#ifdef EADDRNOTAVAIL
    ERRNO(EADDRNOTAVAIL)
#endif
#ifdef ENETDOWN
    ERRNO(ENETDOWN)
#endif
#ifdef ENETUNREACH
    ERRNO(ENETUNREACH)
#endif
#ifdef ENETRESET
    ERRNO(ENETRESET)
#endif
#ifdef ECONNABORTED
    ERRNO(ECONNABORTED)
#endif
#ifdef ECONNRESET
    ERRNO(ECONNRESET)
#endif
#ifdef ENOBUFS
    ERRNO(ENOBUFS)
#endif
#ifdef EISCONN
    ERRNO(EISCONN)
#endif
#ifdef ENOTCONN
    ERRNO(ENOTCONN)
#endif
#ifdef ESHUTDOWN
    ERRNO(ESHUTDOWN)
#endif
#ifdef ETOOMANYREFS
    ERRNO(ETOOMANYREFS)
#endif
#ifdef ETIMEDOUT
    ERRNO(ETIMEDOUT)
#endif
#ifdef ECONNREFUSED
    ERRNO(ECONNREFUSED)
#endif
#ifdef EHOSTDOWN
    ERRNO(EHOSTDOWN)
#endif
#ifdef EHOSTUNREACH
    ERRNO(EHOSTUNREACH)
#endif
#ifdef ELOOP
    ERRNO(ELOOP)
#endif
#ifdef EOVERFLOW
    ERRNO(EOVERFLOW)
#endif
    printf("| x -> Unix.EUNKNOWNERR x\n");

    return 0;
}
