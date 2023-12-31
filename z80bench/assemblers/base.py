import os
import abc
import dload
import tempfile
from timeit import default_timer as timer
from urllib.request import urlopen
import tarfile
from io import BytesIO


# TODO handle fault
class AssemblingResult(object):
	def __init__(self, duration, code):
		self._duration = duration
		self._code = code

	def is_err(self) -> bool:
		return not self.is_ok()
	
	def is_ok(self) -> bool:
		return self._code == 0
	
	def duration(self):
		return self._duration

class Assembler(object):
	def __init__(self, base_location, flavor):
		self._location = os.path.join(base_location, flavor)
		self._flavor = flavor

	def flavor(self):
		return self._flavor
	
	def location(self):
		return self._location
	
	def check_install(self) -> bool:
		if os.path.exists(self.location()):
			print(f">> {self.flavor()} assembler is already installed in {self.location()}")
			return True
		else:
			return False
	
	@abc.abstractmethod
	def install(self):
		"""Install the assembler at the appropriate location"""
		print(f">> Install {self._flavor}")
		os.makedirs(self._location, exist_ok=True)

	@abc.abstractmethod
	def exec_path(self) -> str:
		raise NotImplemented

	def cargo_install(self, crate=None, path=None, features=None):

		if features is None:
			features_arg = ""
		else:
			features_arg = "--features="+(",".join(features))

		if path is None:
			os.system(f"cargo install \"{crate}\" --root=\"{self._location}\" {features_arg}")
		else:
			assert crate is None
			line = f"cargo install --path \"{path}\" --root=\"{self._location}\" {features_arg}"
			res = os.system(line)
			assert res == 0


	def unwrap_http_archive(self, url: str):
		if url.endswith(".zip"):
			dload.save_unzip(url, self.location())
		elif url.endswith(".tar.gz") or url.endswith(".tgz"):
			r = urlopen(url)
			t = tarfile.open(name=None, fileobj=BytesIO(r.read()))
			print(self.location())
			t.extractall(self.location())
			t.close()
		else:
			raise NotImplementedError("Unable to extract format")

	@abc.abstractmethod
	def version_option(self) -> str:
		return "--version"
	
	@abc.abstractmethod
	def version(self)-> str:
		os.system(f"{self.exec_path()} {self.version_option()}")
	
	@abc.abstractmethod
	def build(self, fname, includes) -> AssemblingResult:
		tf = tempfile.NamedTemporaryFile(delete=False)
		tf.close()

		cmd_line = self.build_cmd_line(fname, tf.name, includes)
		print(f">> {cmd_line}")

		start = timer()
		code = os.system(f"{cmd_line} > /dev/null 2>/dev/null")
		end = timer()

		os.unlink(tf.name)

		return AssemblingResult(end-start, code)

	@abc.abstractmethod
	def build_cmd_line(self, ifname,  ofname, includes):
		if includes:
			includes_arg = " ".join(f"-I\"{i}\""  for i in includes)
		else:
			includes_arg = ""
		return f"{self.exec_path()} \"{ifname}\" -o \"{ofname}\" {includes_arg}"