import {Elm} from './Main.elm';
import dictUrl from './dict.tsv';

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
