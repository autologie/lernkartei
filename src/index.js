import "babel-polyfill";
import { Elm } from "./Main.elm";
import { register } from "register-service-worker";
import firebase from "@firebase/app";
import "@firebase/auth";
import "@firebase/firestore";
import copy from "copy-text-to-clipboard";

(async () => {
  firebase.initializeApp({
    apiKey: process.env.FIREBASE_API_KEY,
    authDomain: `${process.env.FIREBASE_PROJECT_ID}.firebaseapp.com`,
    projectId: process.env.FIREBASE_PROJECT_ID
  });

  firebase.firestore().settings({ timestampsInSnapshots: true });

  await firebase.firestore().enablePersistence();

  const db = firebase.firestore();
  const app = Elm.Main.init({ flags: Date.now() });

  firebase.auth().onAuthStateChanged(async user => {
    if (user) {
      app.ports.signInDone.send(user.uid);

      const snapshot = await db
        .collection("users")
        .doc(user.uid)
        .collection("entries")
        .get();

      app.ports.dictionaryLoaded.send(
        snapshot.docs.map(doc => ({ id: doc.id, ...doc.data() }))
      );
    } else {
      await firebase
        .auth()
        .setPersistence(firebase.auth.Auth.Persistence.SESSION);
      await firebase
        .auth()
        .signInWithRedirect(new firebase.auth.GoogleAuthProvider());
    }
  });

  app.ports.saveEntry.subscribe(async ([userId, { id, ...data }]) => {
    await db
      .collection("users")
      .doc(userId)
      .collection("entries")
      .doc(id)
      .set(data);

    app.ports.syncEntryDone.send(null);
  });

  app.ports.deleteEntry.subscribe(async ([userId, entryId]) => {
    await db
      .collection("users")
      .doc(userId)
      .collection("entries")
      .doc(entryId)
      .delete();

    app.ports.syncEntryDone.send(null);
  });

  window.addEventListener("scroll", () =>
    app.ports.scrollChange.send(window.scrollY)
  );

  app.ports.copyToClipboard.subscribe(text => {
    if (!copy(text)) return;

    app.ports.copyToClipboardDone.send(null);
  });

  if (process.env.SERVICE_WORKER_ENABLED === "true") {
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
  }
})();
