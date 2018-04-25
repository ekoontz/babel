UPDATE vocab_item SET source='provided that (condition)' WHERE source = 'provided that -condition-' AND active=true;
UPDATE vocab_item SET source='now (a...)' WHERE source = 'now - a...' AND active=true;
UPDATE vocab_item SET source='so that' WHERE target='affinché' AND active=true;
DELETE FROM vocab_item WHERE target='basso' AND source='short -vs tall' AND active=true;
UPDATE vocab_item SET source='forest, words (sing.)' WHERE source='forest,woods' AND active=true;
UPDATE vocab_item SET source='to sparkle, shine' WHERE source='to sparkle,shine' AND active=true;
UPDATE vocab_item SET source='caraffe, pitcher' WHERE source='caraffe,pitcher' AND active=true;
UPDATE vocab_item SET source='cascade, waterfall' WHERE source='cascade,waterfall' AND active=true;
UPDATE vocab_item SET source='single (for a man)' WHERE source='male-single' AND active=true;
UPDATE vocab_item SET source='what time is it? (sing.)' WHERE source='what time is it?-sing.' AND active=true;
UPDATE vocab_item SET source='what time is it? (plur.)' WHERE source='what time is it?-plur.' AND active=true;
UPDATE vocab_item SET source='cerubic' WHERE source='chubby (term of endearment)' AND active=true;
UPDATE vocab_item SET source='tuft (of hair)' WHERE source='hair - tuft' AND active=true;

SRC 'that which -c...'    'that which (c...)'

UPDATE vocab_item SET active=false
  WHERE target='cognato'
    AND source='brother in law'
    AND category='noun1';

INSERT INTO vocab_item (source,target,category,target_language,source_language)
  VALUES ('cognata','sister in law','nounsingf');

INSERT INTO vocab_item (source,target,category,target_language,source_language)
  VALUES ('cognate','sisters in law','nounplurf');

INSERT INTO vocab_item (source,target,category,target_language,source_language)
  VALUES ('cognato','brother in law','nounsingm');

INSERT INTO vocab_item (source,target,category,target_language,source_language)
  VALUES ('cognati','brothers in law','nounplurm');

SRC 'short - vs long' 'short (vs long)'
SRC 'what does it mean - 3 words'
SRC 'what does it mean (3 words)'

DELETE FROM vocab_item WHERE category='cuisine';
SRC 'kitchen' 'cuisine, kitchen'
SRC 'right -vs left' 'right (vs left)'
DELETE FROM vocab_item WHERE category='of-average-height';
INSERT INTO vocab_item 
    VALUES ('di media statura','of average height','phrase');
    'straight -straight ahead'
'straight, straight ahead'
CAT story-issue-problem story, issue, problem
DELETE FROM vocab_item WHERE source='facile' AND cat='adj1'
SRC 'to make the tour/go around' 'to make the tour, to go around'
SRC 'to stock food, 3 words' 'to stock food (3 words)'
SRC 'hair - bang' 'bang (hair on the forehead)'
SRC 'To search/frisk' 'To search, to frisk'
SRC 'fruit - in general' 'fruit (in general)'

UPDATE vocab_item SET active=false
  WHERE target='genero'
    AND source='son in law'
    AND category='noun1';

INSERT INTO vocab_item (source,target,category,target_language,source_language)
  VALUES ('genero','son in law','nounsingm');

INSERT INTO vocab_item (source,target,category,target_language,source_language)
  VALUES ('generi','sons in law','nounplurm');

SRC 'short for "my parents"' 'my parents (short form)'
SRC 'around, int...' 'around (int... )'
SRC 'there/over there (-ì)' 'there, over there (-ì)'

SRC 'sweater , 6 letters' 'sweater (6 letters)'
SRC 'sweater , 8 letters' 'sweater (8 letters)'

SRC 'better -adj' 'better (adj)'
SRC 'not even -...o' 'not even (...o)'
SRC 'not even -...e' 'not even (...e)'
SRC 'nothing -...e' 'nothing (...e)'
SRC 'single -f.' 'single (for a woman)'

UPDATE vocab_item SET active=false
  WHERE target='nuoro'
    AND source='daughter in law'
    AND category='noun1';

INSERT INTO vocab_item (source,target,category,target_language,source_language)
  VALUES ('nuora','daughter in law','nounsingf');

INSERT INTO vocab_item (source,target,category,target_language,source_language)
  VALUES ('nuore','daughters in law','nounplurf');

SRC 'or -6 letters' 'or (6 letters)'
SRC '(male) observer' 'observer (male)'
SRC '(fem.) observer' 'observer (fem.)'
SRC 'please (-ia)' 'please (2 words -ia)'
DELETE FROM vocab_item WHERE source='very bad' AND category='superlative'
SRC 'very bad - p' 'very bad (p...)'

SRC 'painter, m.' 'painter (masc.)'
SRC 'painter, f.' 'painter (femm.)'

UPDATE vocab_item SET category='adjinv--adjinv' WHERE category='adjinv';

SRC 'also, too, p...' 'also, too (p...)'
SRC 'here, ...-a','here (...-a)'

SRC 'this, fem.' 'this (fem.)'
SRC 'these, fem.' 'these (fem.)'
SRC 'this, masc.' 'this (masc.)'
SRC 'these, masc.' 'these (masc.)'

SRC 'here, ...-i' 'here (...-i)'
SRC 'to shave (r-)' 'to shave (r-...)'
SRC 'writer, masc.' 'writer (masc.)'
SRC 'writer, fem.' 'writer (fem.)'

SRC 'sculptor, m.' 'sculptor (m.)'
SRC 'sculptor, f.' 'sculptor (f.)'

SRC 'safe/sure' 'safe, sure'
SRC 'only -4 letters' 'only (4 letters)'
SRC 'under/below/underneath' 'under, below, underneath'
SRC 'to spread (food, not rumors)' 'to spread (food, not rumors)'

DELETE FROM vocab_item WHERE category='father-in-law';
INSERT INTO vocab_item (source,target,category,target_language,source_language)
  VALUES ('suocera','mother in law','nounsingf');

INSERT INTO vocab_item (source,target,category,target_language,source_language)
  VALUES ('suocere','mothers in law','nounplurf');

INSERT INTO vocab_item (source,target,category,target_language,source_language)
  VALUES ('suocero','father in law','nounsingm');

INSERT INTO vocab_item (source,target,category,target_language,source_language)
  VALUES ('suoceri','fathers in law','nounplurm');


