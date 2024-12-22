import abilang

while True:
    text=input('abilang > ')
    result,error=abilang.run('<stdin>',text)
    
    if error:
        print(error.as_string())
    else:
        print(result)
