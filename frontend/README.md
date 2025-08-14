# Frontend (Elm)

Minimal Elm SPA skeleton fetching the workout plan from `/api/plan`.

## Prerequisites
- Install Elm 0.19.1 (`npm install -g elm` or via your package manager)

## Build
```bash
cd frontend
elm make src/Main.elm --output=../dist/elm.js
```
Then ensure the backend serves `dist/elm.js` (or manually copy into a static folder served by Warp later). For now you can open `frontend/index.html` and adjust the script path if needed.

## Next Steps
- Add routing (dashboard vs active workout)
- Add logging forms and POST calls
- Introduce offline queue and service worker
- Style with a CSS framework
