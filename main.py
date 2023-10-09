from z80bench.assemblers.basm import Basm
from z80bench.assemblers.rasm import Rasm
from z80bench.assemblers.sjasmplus import Sjasmplus
from z80bench.bench.source import Project, Sources
from z80bench.bench.assemblers import Assemblers
from z80bench.benchmark import Bench

import tempfile


DEBUG_MODE=True

with tempfile.TemporaryDirectory() as out_dir:
	#out_dir = "/tmp/tempo"

	print(f"> Working directory: {out_dir}")

	# Build the assemblers list to test
	assemblers = Assemblers()
	assemblers.add_assembler(Basm(out_dir))
	assemblers.add_assembler(Rasm(out_dir))
	assemblers.add_assembler(Sjasmplus(out_dir))

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


	bench = Bench(out_dir, assemblers, sources)
	bench.install()

	bench.versions()

	bench.run()