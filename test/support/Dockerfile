# A Docker container which boots into systemd. We need something with a recent
# glibc version (which excludes Debian) and a functioning journal (not Arch).
#
# It might be possible to use a custom NixOS container, but Fedora appears to
# be good enough.

FROM fedora:38
RUN dnf install -qy systemd nginx && dnf clean all
CMD [ "/lib/systemd/systemd" ]
