from .base import Assembler
import os

CRATE_NAME="cpclib-basm"

class Basm(Assembler):
	def __init__(self, base_location):
		super().__init__(base_location, "basm")

	def install(self):
		if self.check_install(): return
		
		super().install()
		self.cargo_install(CRATE_NAME)

	def exec_path(self) -> str:
		return os.path.join(self.location(), "bin", "basm")