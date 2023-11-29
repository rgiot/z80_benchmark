from z80bench.assemblers import *

from z80bench.bench import *
from z80bench.bench.source import Project, Sources
from z80bench.bench.assemblers import Assemblers
from z80bench.benchmark import Bench

import tempfile
import socket
import sys
import argparse


parser = argparse.ArgumentParser(
                    prog='z80bench',
                    description='A (not yet successfull) tentative to compare z80 assembler on various source code',
                    )

parser.add_argument('-d', '--debug',
                    action='store_true',
					default=False)  
parser.add_argument('-a', '--assembler')
parser.add_argument('-t', '--test')
parser.add_argument('-r', '--repetitions', default=50, type=int)

args = parser.parse_args()
print(args)

DEBUG_MODE=args.debug
NB_REPEAT=args.repetitions
with tempfile.TemporaryDirectory() as out_dir:
	if DEBUG_MODE:
		out_dir = "/tmp/tempo"
		NB_REPEAT = 2


	print(f"> Working directory: {out_dir}")

	# Build the assemblers list to test
	assemblers = Assemblers(args.assembler)
	assemblers.add_assembler(Basm(out_dir))
	if socket.gethostname() == "hibbert":
		assemblers.add_assembler(Basm(out_dir, kind="basm_dev"))

	assemblers.add_assembler(Rasm(out_dir))
	assemblers.add_assembler(Sjasmplus(out_dir))
	assemblers.add_assembler(Sjasm(out_dir))
	assemblers.add_assembler(Pasmo(out_dir))
	assemblers.add_assembler(Vasm(out_dir))
	assemblers.add_assembler(WlaDx(out_dir))
	assemblers.add_assembler(PyZ80(out_dir))

	# build the programs list to test
	sources = Sources()

	# Add tiny stuff
	sources.add_project(Project("./z80/all_instructions.asm")) # Sadly this file is completely wrong
	sources.add_project(Project("./z80/all_instructions_valid.asm")) # Sadly this file is completely wrong
	sources.add_project(Project("./z80/hello_world_cpc.asm")) 
	sources.add_project(Project("./z80/head_over_heels.asm")) 
	sources.add_project(Project("./z80/impossaball.asm")) 


	# TODO Add bigger projects, but compatible with all assemblers
	sources.add_project(Project("./z80/include_files.asm")) 

	sources.add_group(MSXProjectsGenerator())
	sources.add_group(MetalGearProjectsGenerator())
	sources.add_group(Youkaiyashiki())
	sources.add_group(LpfpProjectsGenerator())
	sources.add_group(CPC6128FirmwareProjectsGenerator())
	sources.add_group(AmstradBasicProjectsGenerator())
	sources.add_group(MuckyPawsProjectsGenerator())
	sources.add_group(DotCommandsProjectsGenerator())
	sources.add_group(ZxLuaProjectsGenerator())
	sources.add_group(Z80CryptosProjectsGenerator())
	sources.add_group(Z80VmProjectsGenerator())

	if False:
		print(sources._projects)
		quit()

	bench = Bench(out_dir, assemblers, sources, NB_REPEAT)
	bench.install()
	#quit()

	bench.versions()


	bench.run(filter_tests=args.test)