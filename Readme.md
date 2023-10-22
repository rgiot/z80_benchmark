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

| Source                                                | basm              | basm_dev          | pasmo             | pyz80    | rasm              | sjasm             | sjasmplus         | vasm              | wladx             |
|:------------------------------------------------------|:------------------|:------------------|:------------------|:---------|:------------------|:------------------|:------------------|:------------------|:------------------|
| ALLPUSH.ASM                                           | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | 0.00078 (0.00005) |
| BOREHD.ASM                                            | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | 0.00078 (0.00008) |
| CHCOPY2.ASM                                           | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | 0.00078 (0.00005) |
| DENYINIT.ASM                                          | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | 0.00079 (0.00005) |
| DENYOETH.ASM                                          | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | 0.00078 (0.00005) |
| DENYOROM.ASM                                          | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | 0.00080 (0.00006) |
| DENYOTCP.ASM                                          | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | 0.00080 (0.00007) |
| DES.ASM                                               | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | 0.00081 (0.00007) |
| DISK-ENG.ASM                                          | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | 0.00078 (0.00006) |
| DISK.ASM                                              | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | 0.00079 (0.00006) |
| ESQUIVAT.ASM                                          | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | 0.00077 (0.00005) |
| HMAC.ASM                                              | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | 0.00080 (0.00006) |
| INL-RES.ASM                                           | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | 0.00077 (0.00004) |
| INL-TRAN.ASM                                          | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | 0.00079 (0.00006) |
| MAPMEM.ASM                                            | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | 0.00080 (0.00006) |
| MBCHAR.ASM                                            | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | 0.00078 (0.00005) |
| MD5_SHA1.ASM                                          | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | 0.00079 (0.00006) |
| MEM.ASM                                               | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | 0.00076 (0.00006) |
| MEMDIN.ASM                                            | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | 0.00077 (0.00005) |
| MEMORY.ASM                                            | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | 0.00078 (0.00005) |
| MENUS.ASM                                             | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | 0.00077 (0.00005) |
| MISC.ASM                                              | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | 0.00078 (0.00004) |
| MKROMD1.ASM                                           | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | 0.00077 (0.00007) |
| MKROMD1D.ASM                                          | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | 0.00079 (0.00006) |
| MKROMD23.ASM                                          | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | 0.00079 (0.00005) |
| MKROMDSK.ASM                                          | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | 0.00078 (0.00007) |
| NCAR#1.ASM                                            | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | 0.00078 (0.00006) |
| NCHORR.ASM                                            | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | 0.00077 (0.00004) |
| NDICREAT.ASM                                          | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | 0.00080 (0.00005) |
| NESTAC.ASM                                            | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | 0.00077 (0.00005) |
| NMAN122.ASM                                           | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | 0.00080 (0.00006) |
| NMAN13.ASM                                            | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | 0.00078 (0.00006) |
| NUMTOASC.ASM                                          | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | 0.00076 (0.00005) |
| NUMTOENG.ASM                                          | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | 0.00078 (0.00005) |
| PARCHERR.ASM                                          | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | 0.00077 (0.00005) |
| PRF.ASM                                               | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | 0.00079 (0.00007) |
| PUTSLOT0.ASM                                          | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | 0.00078 (0.00005) |
| RAMDD.ASM                                             | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | 0.00080 (0.00009) |
| REL2NRP.ASM                                           | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | 0.00079 (0.00005) |
| RMV1B.ASM                                             | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | 0.00079 (0.00006) |
| TEXTOR.ASM                                            | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | 0.00078 (0.00005) |
| VRAM.ASM                                              | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | 0.00078 (0.00008) |
| ZEXE.ASM                                              | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | 0.00079 (0.00008) |
| z80/AmstradBasic/Main.asm                             | Failure.          | Failure.          | Failure.          | Failure. | 0.02089 (0.00036) | Failure.          | Failure.          | Failure.          | 0.00079 (0.00006) |
| z80/CPC6128-Firmware-Source/Main.asm                  | Failure.          | Failure.          | Failure.          | Failure. | 0.01955 (0.00055) | Failure.          | Failure.          | Failure.          | 0.00079 (0.00005) |
| z80/MetalGear/MetalGear.asm                           | Failure.          | 0.60758 (0.06051) | Failure.          | Failure. | Failure.          | 0.08281 (0.00132) | Failure.          | Failure.          | 0.00078 (0.00009) |
| z80/all_instructions.asm                              | 0.02698 (0.00199) | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | 0.00086 (0.00006) |
| z80/all_instructions_valid.asm                        | 0.02686 (0.00331) | 0.02898 (0.00234) | Failure.          | Failure. | 0.00454 (0.00017) | 0.00227 (0.00009) | 0.00253 (0.00010) | 0.00206 (0.00008) | 0.00082 (0.00007) |
| z80/dot-commands/close.asm                            | 0.01128 (0.00088) | 0.01299 (0.00125) | Failure.          | Failure. | Failure.          | 0.00175 (0.00012) | 0.00194 (0.00024) | Failure.          | 0.00078 (0.00008) |
| z80/dot-commands/cp-esxdos087.asm                     | 0.01174 (0.00096) | 0.01294 (0.00089) | Failure.          | Failure. | Failure.          | 0.00172 (0.00008) | 0.00196 (0.00012) | Failure.          | 0.00077 (0.00005) |
| z80/dot-commands/cp.asm                               | 0.01202 (0.00221) | 0.01386 (0.00116) | Failure.          | Failure. | Failure.          | 0.00174 (0.00008) | 0.00198 (0.00028) | Failure.          | 0.00078 (0.00014) |
| z80/dot-commands/load.asm                             | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | 0.00225 (0.00013) | 0.00275 (0.00015) | Failure.          | 0.00077 (0.00005) |
| z80/dot-commands/merge.asm                            | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | 0.00223 (0.00015) | 0.00273 (0.00012) | Failure.          | 0.00077 (0.00006) |
| z80/dot-commands/mv-esxdos087.asm                     | 0.00936 (0.00289) | 0.00961 (0.00067) | Failure.          | Failure. | Failure.          | 0.00164 (0.00009) | 0.00175 (0.00009) | Failure.          | 0.00079 (0.00007) |
| z80/dot-commands/open.asm                             | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | 0.00191 (0.00010) | 0.00214 (0.00012) | Failure.          | 0.00077 (0.00006) |
| z80/dot-commands/save.asm                             | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | 0.00212 (0.00014) | 0.00263 (0.00030) | Failure.          | 0.00078 (0.00006) |
| z80/dot-commands/sc2.asm                              | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | 0.00210 (0.00009) | 0.00244 (0.00013) | Failure.          | 0.00078 (0.00006) |
| z80/dot-commands/te.asm                               | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | 0.00220 (0.00012) | 0.00259 (0.00012) | Failure.          | 0.00077 (0.00005) |
| z80/head_over_heels.asm                               | Failure.          | Failure.          | Failure.          | Failure. | 0.01668 (0.00035) | Failure.          | Failure.          | Failure.          | 0.00080 (0.00008) |
| z80/hello_world_cpc.asm                               | 0.00414 (0.00067) | 0.00400 (0.00027) | 0.00117 (0.00006) | Failure. | 0.00399 (0.00021) | 0.00127 (0.00009) | 0.00134 (0.00009) | 0.00091 (0.00008) | 0.00080 (0.00007) |
| z80/impossaball.asm                                   | Failure.          | Failure.          | Failure.          | Failure. | 0.01254 (0.00069) | Failure.          | Failure.          | Failure.          | 0.00080 (0.00006) |
| z80/include_files.asm                                 | 0.04636 (0.00288) | 0.05096 (0.00313) | Failure.          | Failure. | 0.01545 (0.00137) | 0.03918 (0.00097) | 0.04427 (0.00088) | 0.04309 (0.00080) | 0.00080 (0.00005) |
| z80/lpfp/lpfp/addtest.asm                             | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | 0.00079 (0.00006) |
| z80/lpfp/lpfp/subtest.asm                             | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | 0.00079 (0.00006) |
| z80/lpfp/raytracing/spectrum.asm                      | Failure.          | 0.02272 (0.00279) | Failure.          | Failure. | Failure.          | 0.00297 (0.00017) | 0.00375 (0.00021) | Failure.          | 0.00079 (0.00005) |
| z80/muckypaws/Assembly/Jacelock/Jacelock.asm          | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | 0.00078 (0.00005) |
| z80/muckypaws/Assembly/Stix/Stix.asm                  | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | 0.00077 (0.00004) |
| z80/muckypaws/Assembly/Tetris/Tetris.asm              | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | 0.00078 (0.00005) |
| z80/muckypaws/Assembly/Unknown/Sniper.asm             | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | 0.00078 (0.00006) |
| z80/muckypaws/Commercial/Misc/AllHandX.asm            | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | Failure.          | Failure.          | 0.00078 (0.00005) |
| z80/muckypaws/Commercial/Nirvana/NirvanaS48Copier.asm | 0.00806 (0.00094) | 0.00944 (0.00070) | 0.00143 (0.00010) | Failure. | 0.00426 (0.00025) | 0.00144 (0.00008) | 0.00157 (0.00010) | 0.00145 (0.00008) | 0.00078 (0.00006) |
| z80/youkaiyashiki/original/YoukaiYashiki.asm          | 0.56813 (0.01374) | Failure.          | 0.04192 (0.00130) | Failure. | Failure.          | Failure.          | Failure.          | 0.04496 (0.00054) | 0.00079 (0.00006) |
| z80/z80-vm/tiny/main.asm                              | Failure.          | Failure.          | Failure.          | Failure. | 0.00751 (0.00037) | 0.00411 (0.00012) | 0.00517 (0.00012) | Failure.          | 0.00077 (0.00005) |
| z80/z80_crypto/bigmul_test.asm                        | Failure.          | 0.01680 (0.00151) | Failure.          | Failure. | 0.00446 (0.00029) | 0.00181 (0.00009) | 0.00206 (0.00010) | 0.00189 (0.00007) | 0.00079 (0.00006) |
| z80/z80_crypto/keccak_test.asm                        | 0.02149 (0.00160) | 0.02605 (0.00115) | Failure.          | Failure. | 0.00454 (0.00029) | 0.00198 (0.00009) | 0.00226 (0.00009) | Failure.          | 0.00078 (0.00007) |
| z80/z80_crypto/secp256k1_test.asm                     | Failure.          | 0.20166 (0.00921) | Failure.          | Failure. | 0.00879 (0.00045) | 0.01118 (0.00016) | 0.01584 (0.00026) | Failure.          | 0.00077 (0.00006) |
| z80/zx_lua/userlua.asm                                | Failure.          | Failure.          | Failure.          | Failure. | Failure.          | Failure.          | 0.00229 (0.00013) | Failure.          | 0.00079 (0.00006) |

## Success rate over assemblers

| Assembler   |         0 |
|:------------|----------:|
| basm        | 0.142857  |
| basm_dev    | 0.168831  |
| pasmo       | 0.038961  |
| pyz80       | 0         |
| rasm        | 0.155844  |
| sjasm       | 0.25974   |
| sjasmplus   | 0.25974   |
| vasm        | 0.0779221 |
| wladx       | 1         |

## Success rate over projects

| Source                                                |        0 |
|:------------------------------------------------------|---------:|
| ALLPUSH.ASM                                           | 0.111111 |
| BOREHD.ASM                                            | 0.111111 |
| CHCOPY2.ASM                                           | 0.111111 |
| DENYINIT.ASM                                          | 0.111111 |
| DENYOETH.ASM                                          | 0.111111 |
| DENYOROM.ASM                                          | 0.111111 |
| DENYOTCP.ASM                                          | 0.111111 |
| DES.ASM                                               | 0.111111 |
| DISK-ENG.ASM                                          | 0.111111 |
| DISK.ASM                                              | 0.111111 |
| ESQUIVAT.ASM                                          | 0.111111 |
| HMAC.ASM                                              | 0.111111 |
| INL-RES.ASM                                           | 0.111111 |
| INL-TRAN.ASM                                          | 0.111111 |
| MAPMEM.ASM                                            | 0.111111 |
| MBCHAR.ASM                                            | 0.111111 |
| MD5_SHA1.ASM                                          | 0.111111 |
| MEM.ASM                                               | 0.111111 |
| MEMDIN.ASM                                            | 0.111111 |
| MEMORY.ASM                                            | 0.111111 |
| MENUS.ASM                                             | 0.111111 |
| MISC.ASM                                              | 0.111111 |
| MKROMD1.ASM                                           | 0.111111 |
| MKROMD1D.ASM                                          | 0.111111 |
| MKROMD23.ASM                                          | 0.111111 |
| MKROMDSK.ASM                                          | 0.111111 |
| NCAR#1.ASM                                            | 0.111111 |
| NCHORR.ASM                                            | 0.111111 |
| NDICREAT.ASM                                          | 0.111111 |
| NESTAC.ASM                                            | 0.111111 |
| NMAN122.ASM                                           | 0.111111 |
| NMAN13.ASM                                            | 0.111111 |
| NUMTOASC.ASM                                          | 0.111111 |
| NUMTOENG.ASM                                          | 0.111111 |
| PARCHERR.ASM                                          | 0.111111 |
| PRF.ASM                                               | 0.111111 |
| PUTSLOT0.ASM                                          | 0.111111 |
| RAMDD.ASM                                             | 0.111111 |
| REL2NRP.ASM                                           | 0.111111 |
| RMV1B.ASM                                             | 0.111111 |
| TEXTOR.ASM                                            | 0.111111 |
| VRAM.ASM                                              | 0.111111 |
| ZEXE.ASM                                              | 0.111111 |
| z80/AmstradBasic/Main.asm                             | 0.222222 |
| z80/CPC6128-Firmware-Source/Main.asm                  | 0.222222 |
| z80/MetalGear/MetalGear.asm                           | 0.333333 |
| z80/all_instructions.asm                              | 0.222222 |
| z80/all_instructions_valid.asm                        | 0.777778 |
| z80/dot-commands/close.asm                            | 0.555556 |
| z80/dot-commands/cp-esxdos087.asm                     | 0.555556 |
| z80/dot-commands/cp.asm                               | 0.555556 |
| z80/dot-commands/load.asm                             | 0.333333 |
| z80/dot-commands/merge.asm                            | 0.333333 |
| z80/dot-commands/mv-esxdos087.asm                     | 0.555556 |
| z80/dot-commands/open.asm                             | 0.333333 |
| z80/dot-commands/save.asm                             | 0.333333 |
| z80/dot-commands/sc2.asm                              | 0.333333 |
| z80/dot-commands/te.asm                               | 0.333333 |
| z80/head_over_heels.asm                               | 0.222222 |
| z80/hello_world_cpc.asm                               | 0.888889 |
| z80/impossaball.asm                                   | 0.222222 |
| z80/include_files.asm                                 | 0.777778 |
| z80/lpfp/lpfp/addtest.asm                             | 0.111111 |
| z80/lpfp/lpfp/subtest.asm                             | 0.111111 |
| z80/lpfp/raytracing/spectrum.asm                      | 0.444444 |
| z80/muckypaws/Assembly/Jacelock/Jacelock.asm          | 0.111111 |
| z80/muckypaws/Assembly/Stix/Stix.asm                  | 0.111111 |
| z80/muckypaws/Assembly/Tetris/Tetris.asm              | 0.111111 |
| z80/muckypaws/Assembly/Unknown/Sniper.asm             | 0.111111 |
| z80/muckypaws/Commercial/Misc/AllHandX.asm            | 0.111111 |
| z80/muckypaws/Commercial/Nirvana/NirvanaS48Copier.asm | 0.888889 |
| z80/youkaiyashiki/original/YoukaiYashiki.asm          | 0.444444 |
| z80/z80-vm/tiny/main.asm                              | 0.444444 |
| z80/z80_crypto/bigmul_test.asm                        | 0.666667 |
| z80/z80_crypto/keccak_test.asm                        | 0.666667 |
| z80/z80_crypto/secp256k1_test.asm                     | 0.555556 |
| z80/zx_lua/userlua.asm                                | 0.222222 |
