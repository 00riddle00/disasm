## Testavimas Dosbox aplinkoje:

```
> COM.BAT DISASM                      ; sukuriamas DISASM.EXE
> MAKE_COM.BAT INPUT                  ; sukuriamas INPUT.COM
> DISASM.EXE INPUT.COM OUTPUT.ASM     ; disasembliuojamas INPUT.COM į OUTPUT.ASM
> MAKE_COM.BAT OUTPUT                 ; sukuriamas OUTPUT.COM iš OUTPUT.ASM
```

## Rezultatas:

* OUTPUT.ASM failas turi sutapti su INPUT.ASM, išskyrus komentarus kiekvienos eilutės gale, 
nurodančius komandos skaitliuko adresą ir atpažintos operacijos baitus Hex pavidalu.
  Patikrinimas: `meld INPUT.ASM OUTPUT.ASM`, arba kita programa, atliekanti diff funkciją.
* Taip pat OUTPUT.ASM turi asembliuotis į OUTPUT.COM be klaidų
