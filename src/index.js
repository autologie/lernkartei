import {Elm} from './Main.elm';
import dictUrl from './dict.tsv';
import {register} from 'register-service-worker';

const app = Elm.Main.init({flags: [dictUrl, Date.now()]});

new MutationObserver(mutations => {
  const el = document.getElementById('text');
  const scale = Math.min(
    0.6 * el.parentNode.offsetWidth / el.offsetWidth,
    0.4 * el.parentNode.offsetHeight / el.offsetHeight,
  );
  const x = Math.round((el.parentNode.offsetWidth - el.offsetWidth) / 2);
  const y = Math.round((el.parentNode.offsetHeight - el.offsetHeight) / 2);

  app.ports.textDisposition.send([x, y, scale]);
}).observe(document.documentElement, {
  subtree: true,
  attributes: true,
});

register('/service-worker.js', {
  ready(registration) {
    console.log('Service worker is active.');
  },
  registered(registration) {
    console.log('Service worker has been registered.');
  },
  cached(registration) {
    console.log('Content has been cached for offline use.');
  },
  updatefound(registration) {
    console.log('New content is downloading.');
  },
  updated(registration) {
    console.log('New content is available; please refresh.');
  },
  offline() {
    console.log(
      'No internet connection found. App is running in offline mode.',
    );
  },
  error(error) {
    console.error('Error during service worker registration:', error);
  },
});
