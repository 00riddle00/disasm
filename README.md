## Testavimas Dosbox aplinkoje:

```
> COM.BAT DISASM                      ; sukuriamas DISASM.EXE
> MAKE_COM.BAT INPUT                  ; sukuriamas INPUT.COM
> DISASM.EXE INPUT.COM OUTPUT.ASM     ; disasembliuojamas INPUT.COM į OUTPUT.ASM
> MAKE_COM.BAT OUTPUT                 ; sukuriamas OUTPUT.COM iš OUTPUT.ASM
```

## Rezultatas:

* OUTPUT.ASM failas turi sutapti su INPUT.ASM 
  Patikrinimas: `diff INPUT.ASM OUTPUT.ASM`, arba `meld INPUT.ASM OUTPUT.ASM`
* Taip pat OUTPUT.ASM turi asembliuotis į OUTPUT.COM be klaidų

