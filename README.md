AVX1 support of qemu

This adds AVX1 support (VEX prefixes, 256bit vectors etc.) to the x86 qemu JIT.

Still very experimential and somewhat incomplete, but can run "paranoia" successfully
and some simple test programs. It only works for the user version of qemu currently, as
XSAVE is not supported yet (needed to do AVX context switching with a OS)

This is just a hobby project, no relation to my employer.

Opens:
- Various instructions still missing
- Likely quite a few bugs left
- Against an old version of qemu, would need to be updated for recent versions
- Yes I know the commit messages are not great

Andi Kleen

