from .base import Assembler
import abc
import os

VASM_URL = "http://sun.hasenbraten.de/vasm/release/vasm.tar.gz"

class Vasm(Assembler):
	"""Wrapper  to vasm assembler"""
	def __init__(self, base_location):
		super().__init__(base_location, "vasm")
		self._inner_folder = "vasm"

	def install(self):
		if self.check_install(): return
		
		super().install()
		self.unwrap_http_archive(VASM_URL)
		ret = os.system(f"cd \"{self.location()}/{self._inner_folder}/\" && make -j10 CPU=z80 SYNTAX=oldstyle")
		assert ret == 0

	def exec_path(self) -> str:
		return os.path.join(self.location(), self._inner_folder, "vasmz80_oldstyle")
	