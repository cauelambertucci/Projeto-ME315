# Pacotes instalados pelo pak ("any::pkg") saem no manifest com Source "any" e
# Repository "RSPM" (sem URL), o que o Posit Connect Cloud nao consegue baixar.
# Aponta todos para o Posit Package Manager (CRAN).
import json
URL = "https://packagemanager.posit.co/cran/latest"
m = json.load(open("manifest.json"))
n = 0
for p in m["packages"].values():
    if p.get("Repository") != URL or p.get("Source") != "RSPM":
        p["Source"], p["Repository"] = "RSPM", URL
        n += 1
json.dump(m, open("manifest.json", "w"), indent=2, ensure_ascii=False)
open("manifest.json", "a").write("\n")
print(f"manifest.json: {n} pacote(s) corrigido(s)")
