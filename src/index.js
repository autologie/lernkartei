import {Elm} from './Main.elm';
import dictUrl from './dict.csv';

Elm.Main.init({
  node: document.getElementById('elm'),
  flags: dictUrl,
});
