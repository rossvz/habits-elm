import "./main.css";
import { Elm } from "./Main.elm";
import registerServiceWorker from "./registerServiceWorker";

const app = Elm.Main.init({
  node: document.getElementById("root"),
  flags: {
    apiUrl: process.env.ELM_APP_API_URL,
    userId: localStorage.getItem("userId") || null
  }
});

app.ports.writeToLocalStorage.subscribe(data => {
  Object.entries(data).forEach(([key, value]) =>
    value ? localStorage.setItem(key, value) : localStorage.removeItem(key)
  );
});

registerServiceWorker();
