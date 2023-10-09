# Z80assembler benchmark

Benchmark to compare various z80 assembler. Still in its infency, so many things to do before being usefull.
Only few assembler are currently tested:

- ` basm` https://cpcsdk.github.io/rust.cpclib/basm/
- `rasm` http://rasm.wikidot.com/english-index:home
- `sjamsplus` https://z00m128.github.io/sjasmplus/documentation.html

On very few test cases.

It is expected to add more assemblers and examples. The difficulty being able to build all examples by all assemblers.
Feel free to share updates.



Current results are:


# `./z80/all_instructions.asm`

-       basm: 0.026475(0.002644) *
-       rasm: unable to assemble
-  sjasmplus: unable to assemble

# `./z80/all_instructions_valid.asm`

-       basm: 0.025399(0.001986)
-       rasm: 0.004360(0.000243)
-  sjasmplus: 0.002526(0.000106) *

# `./z80/hello_world_cpc.asm`

-       basm: 0.003856(0.000388)
-       rasm: 0.003839(0.000243)
-  sjasmplus: 0.001333(0.000107) *

# `./z80/head_over_heels.asm`
-       basm: unable to assemble
-       rasm: 0.015923(0.000481) *
-  sjasmplus: unable to assemble

# `./z80/impossaball.asm`
-       basm: unable to assemble
-       rasm: 0.011915(0.000384) *
-  sjasmplus: unable to assemble

# `./z80/include_files.asm`
-       basm: 0.048019(0.002892)
-       rasm: 0.014683(0.000445) *
-  sjasmplus: 0.044069(0.000729)

On this CPU

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