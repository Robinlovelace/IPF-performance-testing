#!/usr/bin/python
import base64, os, os.path, shutil

try:
	entrieslog = open("entries.log", "r")
except IOError:
	print "entries.log not found"
	exit(1)
restored = 0
not_restored = 0
try:
	os.mkdir("restore")
except OSError:
	print "restore folder already exists, will not restore"
	exit(1)

for line in entrieslog:
	parts = tuple(line.split("|"))
	try:
		(filename, path, x, y, z) = parts
		filename = base64.b64decode(filename)
		path = "restore" + base64.b64decode(path).split(":")[1]
		print "Copying %s to %s" % (filename, path)
		folder = os.path.dirname(path)
		try:
			os.makedirs(folder)
		except OSError:
			pass
		shutil.copy2(filename, path)
		restored += 1
	except ValueError:
		print "Format incorrect"
		not_restored += 1
	except IOError:
		print "Unable to restore file"
		not_restored += 1

print "Finished: %d restored, %d not restored" % (restored, not_restored)
