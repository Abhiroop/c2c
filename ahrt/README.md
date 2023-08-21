# ahrt - abhi's haskell runtime

This is my Haskell runtime. 

Goal - Portability. It is currently a pain to run GHC/Haskell anywhere apart from x86 machines. Despite having the LLVM backend, the overall toolchain falls short for nearly all microcontrollers and exotic RISC-V boards. Then there are security architecture extensions such as Intel SGX, ARM TrustZone, CHERI, etc., which motivated this work in the first place.
