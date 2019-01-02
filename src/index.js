import "babel-polyfill";
import { Elm } from "./Main.elm";
import { register } from "register-service-worker";
import firebase from "@firebase/app";
import "@firebase/auth";
import "@firebase/storage";

firebase.initializeApp({
  apiKey: "AIzaSyD22gzLwBBW7Ho6bEsLP9wEy01Wwli22Yk",
  authDomain: "wortkarten.firebaseapp.com",
  projectId: "wortkarten",
  storageBucket: "wortkarten.appspot.com"
});

const app = Elm.Main.init({ flags: Date.now() });

firebase.auth().onAuthStateChanged(async user => {
  if (user) {
    app.ports.signInDone.send(user.uid);
  } else {
    await firebase
      .auth()
      .setPersistence(firebase.auth.Auth.Persistence.SESSION);
    await firebase
      .auth()
      .signInWithPopup(new firebase.auth.GoogleAuthProvider());
  }
});

new MutationObserver(() => {
  const el = document.getElementById("text");
  const scale = Math.min(
    (0.6 * el.parentNode.offsetWidth) / el.offsetWidth,
    (0.4 * el.parentNode.offsetHeight) / el.offsetHeight
  );
  const x = Math.round((el.parentNode.offsetWidth - el.offsetWidth) / 2);
  const y = Math.round((el.parentNode.offsetHeight - el.offsetHeight) / 2);

  app.ports.textDisposition.send([x, y, scale]);
}).observe(document.documentElement, {
  subtree: true,
  attributes: true
});

app.ports.persistDictionary.subscribe(async ([dict, userId]) => {
  await firebase
    .storage()
    .ref()
    .child(`${userId}.tsv`)
    .putString(dict, "raw", {
      contentType: "text/tab-separated-values"
    });

  app.ports.persistDictionaryDone.send(null);
});

app.ports.getDictUrl.subscribe(async userId => {
  const url = await firebase
    .storage()
    .ref()
    .child(`${userId}.tsv`)
    .getDownloadURL();

  app.ports.getDictUrlDone.send(url);
});

/*
register("/service-worker.js", {
  ready(registration) {
    console.log("Service worker is active.");
  },
  registered(registration) {
    console.log("Service worker has been registered.");
  },
  cached(registration) {
    console.log("Content has been cached for offline use.");
  },
  updatefound(registration) {
    console.log("New content is downloading.");
  },
  updated(registration) {
    console.log("New content is available; please refresh.");
  },
  offline() {
    console.log(
      "No internet connection found. App is running in offline mode."
    );
  },
  error(error) {
    console.error("Error during service worker registration:", error);
  }
});
*/
