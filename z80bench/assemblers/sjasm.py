from .base import Assembler
import os
import abc

SJASM_0_39_URL="https://github.com/Konamiman/Sjasm/archive/refs/tags/v0.39h.zip"


class Sjasm(Assembler):
	"""Wrapper to sjasm."""
	def __init__(self, base_location):
		super().__init__(base_location, "sjasm")
		self._inner_folder = "Sjasm-0.39h/Sjasm"


	def install(self):
		if self.check_install(): return
		
		super().install()
		self.unwrap_http_archive(SJASM_0_39_URL)
		ret = os.system(f"cd \"{self.location()}/{self._inner_folder}/\" && mv Sjasm.cpp sjasm.cpp &&  make -f../Makefile -j10 USE_LUA=0")
		assert ret == 0

	def exec_path(self) -> str:
		return os.path.join(self.location(), self._inner_folder, "sjasm")
	

	@abc.abstractmethod
	def build_cmd_line(self, ifname, ofname, includes):
		if includes:
			includes_arg = " ".join(f"-I\"{i}\""  for i in includes)
		else:
			includes_arg = ""
		return f"{self.exec_path()} \"{ifname}\" {includes_arg}"