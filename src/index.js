import {Elm} from './Main.elm';
import dictUrl from './dict.csv';

Elm.Main.init({flags: [dictUrl, Date.now()]});
