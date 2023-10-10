from .base import Assembler
import os
import socket

CRATE_NAME="cpclib-basm"
DEV_PATH = "../rust.cpcdemotools/cpclib-basm/"

class Basm(Assembler):
	def __init__(self, base_location, kind="basm"):
		assert kind in ["basm", "basm_dev"]
		super().__init__(base_location, kind)

	def install(self):
		if self.check_install(): return
		
		super().install()
		if self.flavor() == "basm":
			self.cargo_install(CRATE_NAME)
		else:
			assert socket.gethostname() == "hibbert"
			self.cargo_install(path=DEV_PATH)


	def exec_path(self) -> str:
		return os.path.join(self.location(), "bin", "basm")