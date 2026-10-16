# Deposito slide

Sorgente di <https://enricotoffalini.github.io/slides/>. La pagina è generata da `index.qmd` a partire da `allSlides.csv`.

## Aggiungere una presentazione

1. Copia il file (PDF, oppure la cartella con le slide HTML) nella repo.
2. Aggiungi una riga in `allSlides.csv`:

   ```
   Year,Congress,Title,Link
   2026-10_16,Nome evento,Titolo della presentazione,Toffalini-Evento-2026.pdf
   ```

   - **Year**: `2026`, `2026-10`, `2026-10_16` o `2026-10-16` (la pagina mostra "16 ott", raggruppato per anno; l'ordine è automatico).
   - **Link**: URL completo (`https://enricotoffalini.github.io/slides/...`) oppure solo il percorso del file nella repo (`Toffalini-Evento-2026.pdf`, `cartella/index.html`). Va bene anche un link esterno.
   - Titoli con virgole tra virgolette `"..."`. Si possono usare le lettere accentate.
   - Il CSV può essere salvato anche da Excel (UTF-8 o "CSV delimitato", con `,` o `;`).
3. Renderizza `index.qmd` (RStudio: *Render*, oppure `quarto render index.qmd`).
4. Commit e push: GitHub Actions pubblica tutto.

Durante il render, nella console compare un avviso **ATTENZIONE** se un file indicato nel CSV non esiste nella repo (controllo anche di maiuscole/minuscole, che su GitHub Pages contano) o se una data non è riconosciuta.

I QR code in `qr/` sono generati automaticamente a ogni render (non vanno modificati a mano). Grafica in `styles.css`, font in `fonts/`.
