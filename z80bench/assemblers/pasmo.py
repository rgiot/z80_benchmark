from .base import Assembler
import abc
import os

PASMO_0_5_5_URL = "https://pasmo.speccy.org/bin/pasmo-0.5.5.tar.gz"

class Pasmo(Assembler):
	"""Wrapper to Pasmo assembler"""
	def __init__(self, base_location):
		super().__init__(base_location, "pasmo")
		self._inner_folder = "pasmo-0.5.5"

	def install(self):
		if self.check_install(): return
		
		super().install()
		self.unwrap_http_archive(PASMO_0_5_5_URL)
		ret = os.system(f"cd \"{self.location()}/{self._inner_folder}/\" && ./configure && make -j10")
		assert ret == 0

	def exec_path(self) -> str:
		return os.path.join(self.location(), self._inner_folder, "pasmo")
	