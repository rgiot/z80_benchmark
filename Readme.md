# Z80assembler benchmark

Benchmark to compare various z80 assembler. Still in its infency, so many things to do before being usefull.
Only few assembler are currently tested:

- `basm` https://cpcsdk.github.io/rust.cpclib/basm/. `cargo` from the `rust` toolchain is expected.
- `rasm` http://rasm.wikidot.com/english-index:home. `gcc` and `make` are expected.
- `sjamsplus` https://z00m128.github.io/sjasmplus/documentation.html. `gcc` and `make` are expected. `lua` support is not compiled.
- `vasm`
- `pasmo`
- `wla-dx` 


Dependencies for each of these assembler are expected to build them.

On very few test cases.

It is expected to add more assemblers and examples. The difficulty being able to build all examples by all assemblers.
Feel free to share updates.



Current results follows. Some remarks:

- `./z80/all_instructions.asm` is expected to fail as it contains illegal instructions
- Some pairs (source code, assembler) may fail because I have not (yet) properly set up the appropriate options
- The produced binary is not verified against a groundtruth
- Tests are done on this CPU:

```
Architecture:            x86_64
  CPU op-mode(s):        32-bit, 64-bit
  Address sizes:         39 bits physical, 48 bits virtual
  Byte Order:            Little Endian
CPU(s):                  20
  On-line CPU(s) list:   0-19
Vendor ID:               GenuineIntel
  Model name:            12th Gen Intel(R) Core(TM) i7-12700KF
    CPU family:          6
    Model:               151
    Thread(s) per core:  2
    Core(s) per socket:  12
    Socket(s):           1
    Stepping:            2
    CPU(s) scaling MHz:  69%
    CPU max MHz:         5000.0000
    CPU min MHz:         800.0000
    BogoMIPS:            7219.20
    Flags:               fpu vme de pse tsc msr pae mce cx8 apic sep mtrr pge mca cmov pat pse36 clflush dts acpi mmx fxsr sse sse2 ss ht tm pbe syscall nx pdpe1gb rdtscp lm constant_tsc art arch_perfmon pebs bts rep_good nopl xtopology nonstop_tsc cpuid aperfmperf tsc_known_
                         freq pni pclmulqdq dtes64 monitor ds_cpl vmx est tm2 ssse3 sdbg fma cx16 xtpr pdcm sse4_1 sse4_2 x2apic movbe popcnt tsc_deadline_timer aes xsave avx f16c rdrand lahf_lm abm 3dnowprefetch cpuid_fault epb ssbd ibrs ibpb stibp ibrs_enhanced tpr_shadow v
                         nmi flexpriority ept vpid ept_ad fsgsbase tsc_adjust bmi1 avx2 smep bmi2 erms invpcid rdseed adx smap clflushopt clwb intel_pt sha_ni xsaveopt xsavec xgetbv1 xsaves split_lock_detect avx_vnni dtherm ida arat pln pts hwp hwp_notify hwp_act_window hwp_e
                         pp hwp_pkg_req hfi umip pku ospke waitpkg gfni vaes vpclmulqdq rdpid movdiri movdir64b fsrm md_clear serialize arch_lbr ibt flush_l1d arch_capabilities
Virtualization features:
  Virtualization:        VT-x
Caches (sum of all):
  L1d:                   512 KiB (12 instances)
  L1i:                   512 KiB (12 instances)
  L2:                    12 MiB (9 instances)
  L3:                    25 MiB (1 instance)
NUMA:
  NUMA node(s):          1
  NUMA node0 CPU(s):     0-19
Vulnerabilities:
  Gather data sampling:  Not affected
  Itlb multihit:         Not affected
  L1tf:                  Not affected
  Mds:                   Not affected
  Meltdown:              Not affected
  Mmio stale data:       Not affected
  Retbleed:              Not affected
  Spec rstack overflow:  Not affected
  Spec store bypass:     Mitigation; Speculative Store Bypass disabled via prctl
  Spectre v1:            Mitigation; usercopy/swapgs barriers and __user pointer sanitization
  Spectre v2:            Mitigation; Enhanced IBRS, IBPB conditional, RSB filling, PBRSB-eIBRS SW sequence
  Srbds:                 Not affected
  Tsx async abort:       Not affected
```
## Summary of assembling time

| Source                                                | basm              | basm_dev          | pasmo             | pyz80    | rasm              | sjasm             | sjasmplus         | vasm              | wladx    |
|:------------------------------------------------------|:------------------|:------------------|:------------------|:---------|:------------------|:------------------|:------------------|:------------------|:---------|
| ALLPUSH.ASM                                           | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | Failure. |
| BOREHD.ASM                                            | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | Failure. |
| CHCOPY2.ASM                                           | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | Failure. |
| DENYINIT.ASM                                          | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | Failure. |
| DENYOETH.ASM                                          | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | Failure. |
| DENYOROM.ASM                                          | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | Failure. |
| DENYOTCP.ASM                                          | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | Failure. |
| DES.ASM                                               | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | Failure. |
| DISK-ENG.ASM                                          | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | Failure. |
| DISK.ASM                                              | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | Failure. |
| ESQUIVAT.ASM                                          | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | Failure. |
| HMAC.ASM                                              | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | Failure. |
| INL-RES.ASM                                           | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | Failure. |
| INL-TRAN.ASM                                          | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | Failure. |
| MAPMEM.ASM                                            | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | Failure. |
| MBCHAR.ASM                                            | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | Failure. |
| MD5_SHA1.ASM                                          | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | Failure. |
| MEM.ASM                                               | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | Failure. |
| MEMDIN.ASM                                            | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | Failure. |
| MEMORY.ASM                                            | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | Failure. |
| MENUS.ASM                                             | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | Failure. |
| MISC.ASM                                              | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | Failure. |
| MKROMD1.ASM                                           | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | Failure. |
| MKROMD1D.ASM                                          | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | Failure. |
| MKROMD23.ASM                                          | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | Failure. |
| MKROMDSK.ASM                                          | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | Failure. |
| NCAR#1.ASM                                            | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | Failure. |
| NCHORR.ASM                                            | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | Failure. |
| NDICREAT.ASM                                          | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | Failure. |
| NESTAC.ASM                                            | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | Failure. |
| NMAN122.ASM                                           | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | Failure. |
| NMAN13.ASM                                            | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | Failure. |
| NUMTOASC.ASM                                          | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | Failure. |
| NUMTOENG.ASM                                          | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | Failure. |
| PARCHERR.ASM                                          | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | Failure. |
| PRF.ASM                                               | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | Failure. |
| PUTSLOT0.ASM                                          | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | Failure. |
| RAMDD.ASM                                             | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | Failure. |
| REL2NRP.ASM                                           | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | Failure. |
| RMV1B.ASM                                             | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | Failure. |
| TEXTOR.ASM                                            | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | Failure. |
| VRAM.ASM                                              | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | Failure. |
| ZEXE.ASM                                              | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | Failure. |
| z80/AmstradBasic/Main.asm                             | Failure.          | 0.06441 (0.00314) | Failure.          | Failure. | 0.02019 (0.00082) | Failure.          | Failure.          | Failure.          | Failure. |
| z80/CPC6128-Firmware-Source/Main.asm                  | Failure.          | 0.11748 (0.00485) | Failure.          | Failure. | 0.01871 (0.00068) | Failure.          | Failure.          | Failure.          | Failure. |
| z80/MetalGear/MetalGear.asm                           | Failure.          | 0.81641 (0.08742) | Failure.          | Failure. | Failure.          | 0.08163 (0.00092) | Failure.          | Failure.          | Failure. |
| z80/all_instructions.asm                              | 0.02586 (0.00078) | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | Failure. |
| z80/all_instructions_valid.asm                        | 0.02445 (0.00115) | 0.04466 (0.00475) | Failure.          | Failure. | 0.00424 (0.00016) | 0.00216 (0.00009) | 0.00238 (0.00005) | 0.00193 (0.00005) | Failure. |
| z80/dot-commands/close.asm                            | 0.01041 (0.00089) | 0.01373 (0.00177) | Failure.          | Failure. | Failure.          | 0.00155 (0.00009) | 0.00174 (0.00004) | Failure.          | Failure. |
| z80/dot-commands/cp-esxdos087.asm                     | 0.01070 (0.00050) | 0.01333 (0.00105) | Failure.          | Failure. | Failure.          | 0.00160 (0.00008) | 0.00174 (0.00009) | Failure.          | Failure. |
| z80/dot-commands/cp.asm                               | 0.01125 (0.00129) | 0.01342 (0.00079) | Failure.          | Failure. | Failure.          | 0.00158 (0.00009) | 0.00180 (0.00006) | Failure.          | Failure. |
| z80/dot-commands/load.asm                             | Failure.          | 0.02332 (0.00099) | Failure.          | Failure. | Failure.          | 0.00202 (0.00008) | 0.00253 (0.00007) | Failure.          | Failure. |
| z80/dot-commands/merge.asm                            | Failure.          | 0.02284 (0.00098) | Failure.          | Failure. | Failure.          | 0.00204 (0.00008) | 0.00255 (0.00007) | Failure.          | Failure. |
| z80/dot-commands/mv-esxdos087.asm                     | 0.00822 (0.00061) | 0.00979 (0.00090) | Failure.          | Failure. | Failure.          | 0.00146 (0.00007) | 0.00163 (0.00006) | Failure.          | Failure. |
| z80/dot-commands/open.asm                             | Failure.          | 0.01884 (0.00119) | Failure.          | Failure. | Failure.          | 0.00172 (0.00008) | 0.00198 (0.00008) | Failure.          | Failure. |
| z80/dot-commands/save.asm                             | Failure.          | 0.03337 (0.00179) | Failure.          | Failure. | Failure.          | 0.00193 (0.00006) | 0.00242 (0.00006) | Failure.          | Failure. |
| z80/dot-commands/sc2.asm                              | Failure.          | 0.02889 (0.00106) | Failure.          | Failure. | Failure.          | 0.00190 (0.00010) | 0.00226 (0.00007) | Failure.          | Failure. |
| z80/dot-commands/te.asm                               | Failure.          | 0.03937 (0.00099) | Failure.          | Failure. | Failure.          | 0.00202 (0.00015) | 0.00241 (0.00006) | Failure.          | Failure. |
| z80/head_over_heels.asm                               | Failure.          | Failure.          | Failure.          | Failure. | 0.01602 (0.00031) | Failure.          | Failure.          | Failure.          | Failure. |
| z80/hello_world_cpc.asm                               | 0.00369 (0.00030) | 0.00410 (0.00036) | 0.00105 (0.00005) | Failure. | 0.00361 (0.00017) | 0.00117 (0.00007) | 0.00123 (0.00009) | 0.00079 (0.00007) | Failure. |
| z80/impossaball.asm                                   | Failure.          | Failure.          | Failure.          | Failure. | 0.01184 (0.00029) | Failure.          | Failure.          | Failure.          | Failure. |
| z80/include_files.asm                                 | 0.04587 (0.00313) | 0.07500 (0.00772) | Failure.          | Failure. | 0.01462 (0.00052) | 0.03838 (0.00073) | 0.04291 (0.00088) | 0.04222 (0.00085) | Failure. |
| z80/lpfp/raytracing/spectrum.asm                      | Failure.          | 0.02588 (0.00199) | Failure.          | Failure. | Failure.          | 0.00273 (0.00009) | 0.00351 (0.00007) | Failure.          | Failure. |
| z80/lpfp/test/addtest.asm                             | Failure.          | 0.04595 (0.00091) | Failure.          | Failure. | 0.00519 (0.00059) | Failure.          | Failure.          | Failure.          | Failure. |
| z80/lpfp/test/subtest.asm                             | Failure.          | 0.04574 (0.00111) | Failure.          | Failure. | 0.00497 (0.00019) | Failure.          | Failure.          | Failure.          | Failure. |
| z80/muckypaws/Assembly/Jacelock/Jacelock.asm          | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | Failure. |
| z80/muckypaws/Assembly/Stix/Stix.asm                  | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | Failure. |
| z80/muckypaws/Assembly/Tetris/Tetris.asm              | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | Failure. |
| z80/muckypaws/Assembly/Unknown/Sniper.asm             | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | Failure. |
| z80/muckypaws/Commercial/Misc/AllHandX.asm            | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | Failure. |
| z80/muckypaws/Commercial/Nirvana/NirvanaS48Copier.asm | 0.00734 (0.00043) | 0.01163 (0.00105) | 0.00131 (0.00008) | Failure. | 0.00390 (0.00019) | 0.00130 (0.00004) | 0.00141 (0.00007) | 0.00135 (0.00006) | Failure. |
| z80/youkaiyashiki/original/YoukaiYashiki.asm          | 0.56570 (0.01856) | Failure.          | 0.04039 (0.00058) | Failure. | Failure.          | Failure.          | Failure.          | 0.04400 (0.00070) | Failure. |
| z80/z80-vm/tiny/main.asm                              | Failure.          | Failure.          | Failure.          | Failure. | 0.00685 (0.00030) | 0.00387 (0.00011) | 0.00500 (0.00042) | Failure.          | Failure. |
| z80/z80_crypto/bigmul_test.asm                        | Failure.          | 0.01975 (0.00088) | Failure.          | Failure. | 0.00393 (0.00010) | 0.00167 (0.00010) | 0.00193 (0.00005) | 0.00178 (0.00008) | Failure. |
| z80/z80_crypto/keccak_test.asm                        | 0.02045 (0.00104) | 0.02693 (0.00057) | Failure.          | Failure. | 0.00399 (0.00012) | 0.00182 (0.00011) | 0.00213 (0.00005) | Failure.          | Failure. |
| z80/z80_crypto/secp256k1_test.asm                     | Failure.          | 0.35591 (0.00659) | Failure.          | Failure. | 0.00799 (0.00017) | 0.01088 (0.00023) | 0.01539 (0.00030) | Failure.          | Failure. |
| z80/zx_lua/userlua.asm                                | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | 0.00211 (0.00006) | Failure.          | Failure. |

## Success rate over assemblers

| Assembler   |         0 |
|:------------|----------:|
| basm        | 0.142857  |
| basm_dev    | 0.298701  |
| pasmo       | 0.038961  |
| pyz80       | 0         |
| rasm        | 0.181818  |
| sjasm       | 0.25974   |
| sjasmplus   | 0.25974   |
| vasm        | 0.0779221 |
| wladx       | 0         |

## Success rate over projects

| Source                                                |        0 |
|:------------------------------------------------------|---------:|
| ALLPUSH.ASM                                           | 0        |
| BOREHD.ASM                                            | 0        |
| CHCOPY2.ASM                                           | 0        |
| DENYINIT.ASM                                          | 0        |
| DENYOETH.ASM                                          | 0        |
| DENYOROM.ASM                                          | 0        |
| DENYOTCP.ASM                                          | 0        |
| DES.ASM                                               | 0        |
| DISK-ENG.ASM                                          | 0        |
| DISK.ASM                                              | 0        |
| ESQUIVAT.ASM                                          | 0        |
| HMAC.ASM                                              | 0        |
| INL-RES.ASM                                           | 0        |
| INL-TRAN.ASM                                          | 0        |
| MAPMEM.ASM                                            | 0        |
| MBCHAR.ASM                                            | 0        |
| MD5_SHA1.ASM                                          | 0        |
| MEM.ASM                                               | 0        |
| MEMDIN.ASM                                            | 0        |
| MEMORY.ASM                                            | 0        |
| MENUS.ASM                                             | 0        |
| MISC.ASM                                              | 0        |
| MKROMD1.ASM                                           | 0        |
| MKROMD1D.ASM                                          | 0        |
| MKROMD23.ASM                                          | 0        |
| MKROMDSK.ASM                                          | 0        |
| NCAR#1.ASM                                            | 0        |
| NCHORR.ASM                                            | 0        |
| NDICREAT.ASM                                          | 0        |
| NESTAC.ASM                                            | 0        |
| NMAN122.ASM                                           | 0        |
| NMAN13.ASM                                            | 0        |
| NUMTOASC.ASM                                          | 0        |
| NUMTOENG.ASM                                          | 0        |
| PARCHERR.ASM                                          | 0        |
| PRF.ASM                                               | 0        |
| PUTSLOT0.ASM                                          | 0        |
| RAMDD.ASM                                             | 0        |
| REL2NRP.ASM                                           | 0        |
| RMV1B.ASM                                             | 0        |
| TEXTOR.ASM                                            | 0        |
| VRAM.ASM                                              | 0        |
| ZEXE.ASM                                              | 0        |
| z80/AmstradBasic/Main.asm                             | 0.222222 |
| z80/CPC6128-Firmware-Source/Main.asm                  | 0.222222 |
| z80/MetalGear/MetalGear.asm                           | 0.222222 |
| z80/all_instructions.asm                              | 0.111111 |
| z80/all_instructions_valid.asm                        | 0.666667 |
| z80/dot-commands/close.asm                            | 0.444444 |
| z80/dot-commands/cp-esxdos087.asm                     | 0.444444 |
| z80/dot-commands/cp.asm                               | 0.444444 |
| z80/dot-commands/load.asm                             | 0.333333 |
| z80/dot-commands/merge.asm                            | 0.333333 |
| z80/dot-commands/mv-esxdos087.asm                     | 0.444444 |
| z80/dot-commands/open.asm                             | 0.333333 |
| z80/dot-commands/save.asm                             | 0.333333 |
| z80/dot-commands/sc2.asm                              | 0.333333 |
| z80/dot-commands/te.asm                               | 0.333333 |
| z80/head_over_heels.asm                               | 0.111111 |
| z80/hello_world_cpc.asm                               | 0.777778 |
| z80/impossaball.asm                                   | 0.111111 |
| z80/include_files.asm                                 | 0.666667 |
| z80/lpfp/raytracing/spectrum.asm                      | 0.333333 |
| z80/lpfp/test/addtest.asm                             | 0.222222 |
| z80/lpfp/test/subtest.asm                             | 0.222222 |
| z80/muckypaws/Assembly/Jacelock/Jacelock.asm          | 0        |
| z80/muckypaws/Assembly/Stix/Stix.asm                  | 0        |
| z80/muckypaws/Assembly/Tetris/Tetris.asm              | 0        |
| z80/muckypaws/Assembly/Unknown/Sniper.asm             | 0        |
| z80/muckypaws/Commercial/Misc/AllHandX.asm            | 0        |
| z80/muckypaws/Commercial/Nirvana/NirvanaS48Copier.asm | 0.777778 |
| z80/youkaiyashiki/original/YoukaiYashiki.asm          | 0.333333 |
| z80/z80-vm/tiny/main.asm                              | 0.333333 |
| z80/z80_crypto/bigmul_test.asm                        | 0.555556 |
| z80/z80_crypto/keccak_test.asm                        | 0.555556 |
| z80/z80_crypto/secp256k1_test.asm                     | 0.444444 |
| z80/zx_lua/userlua.asm                                | 0.111111 |
