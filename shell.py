import abilang

while True:
	text = input('abilang > ')
	if text.strip() == "": continue
	result, error = abilang.run('<stdin>', text)

	if error:
		print(error.as_string())
	elif result:
		if len(result.elements) == 1:
			print(repr(result.elements[0]))
		else:
			print(repr(result))
