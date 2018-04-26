BEGIN TRANSACTION;
UPDATE vocab_item SET source='provided that (condition)' WHERE source = 'provided that -condition-';
UPDATE vocab_item SET source='now (a...)' WHERE source = 'now - a...';
UPDATE vocab_item SET source='so that' WHERE target='affinché';
DELETE FROM vocab_item WHERE target='basso' AND source='short -vs tall';
UPDATE vocab_item SET source='forest' WHERE source='forest, woods';
UPDATE vocab_item SET source='to sparkle, shine' WHERE source='to sparkle,shine';
UPDATE vocab_item SET source='caraffe, pitcher' WHERE source='caraffe,pitcher';
UPDATE vocab_item SET source='cascade, waterfall' WHERE source='cascade,waterfall';
UPDATE vocab_item SET source='single (for a man)' WHERE source='male-single';
UPDATE vocab_item SET source='what time is it? (sing.)' WHERE source='what time is it?-sing.';
UPDATE vocab_item SET source='what time is it? (plur.)' WHERE source='what time is it?-plur.';
UPDATE vocab_item SET source='cherubic' WHERE target='cicciottello';
UPDATE vocab_item SET source='tuft (of hair)' WHERE source='hair - tuft';

UPDATE vocab_item SET source='that which (c...)' WHERE source='that which -c...';

UPDATE vocab_item SET active=false
  WHERE target='cognato'
    AND source='brother in law'
    AND structure->>'vocabcoach-category'='noun1';

INSERT INTO vocab_item (target,source,structure,target_language,source_language)
  VALUES ('cognata','sister in law','{"vocabcoach-category":"nounsingf"}'::json,'it','en');

INSERT INTO vocab_item (target,source,structure,target_language,source_language)
  VALUES ('cognate','sisters in law','{"vocabcoach-category":"nounplurf"}'::json,'it','en');

INSERT INTO vocab_item (target,source,structure,target_language,source_language)
  VALUES ('cognato','brother in law','{"vocabcoach-category":"nounsingm"}'::json,'it','en');

INSERT INTO vocab_item (target,source,structure,target_language,source_language)
  VALUES ('cognati','brothers in law','{"vocabcoach-category":"nounplurm"}'::json,'it','en');

UPDATE vocab_item SET source='short (vs long)' WHERE source='short - vs long';
UPDATE vocab_item SET source='what does it mean (3 words)' WHERE source='what does it mean - 3 words';

UPDATE vocab_item SET active=false WHERE structure->>'vocabcoach-category'='cuisine';
UPDATE vocab_item SET source='cuisine, kitchen' WHERE source='kitchen';
UPDATE vocab_item SET source='right (vs left)' WHERE source='right -vs left';
UPDATE vocab_item SET active=false WHERE structure->>'vocabcoach-category'='of-average-height';
INSERT INTO vocab_item (target,source,structure,target_language,source_language)
    VALUES ('di media statura','of average height','{"vocabcoach-category":"phrase"}'::json,'it','en');

UPDATE vocab_item SET source='straight, straight ahead' WHERE source='straight -straight ahead';

UPDATE vocab_item SET active=false WHERE structure->>'vocabcoach-category'='story-issue-problem';
INSERT INTO vocab_item (target,source,structure,target_language,source_language)
    VALUES ('story,issue,problem','faccenda','{"vocabcoach-category":"noun1"}'::json,'it','en');
UPDATE vocab_item SET active=false WHERE source='facile' AND structure->>'vocabcoach-category'='adj1';
UPDATE vocab_item SET source='to make the tour, to go around' WHERE source='to make the tour/go around';
UPDATE vocab_item SET source='to stock food (3 words)' WHERE source='to stock food, 3 words';
UPDATE vocab_item SET source='bang (hair on the forehead)' WHERE source='hair - bang';
UPDATE vocab_item SET source='To search, to frisk' WHERE source='To search/frisk';
UPDATE vocab_item SET source='fruit (in general)' WHERE source='fruit - in general';

UPDATE vocab_item SET active=false 
  WHERE target='genero'
    AND source='son in law'
    AND structure->>'vocabcoach-category'='noun1';

INSERT INTO vocab_item (target,source,structure,target_language,source_language)
  VALUES ('genero','son in law','{"vocabcoach-category":"nounsingm"}'::json,'it','en');

INSERT INTO vocab_item (target,source,structure,target_language,source_language)
  VALUES ('generi','sons in law','{"vocabcoach-category":"nounplurm"}'::json,'it','en');

UPDATE vocab_item SET source='my parents (short form)' WHERE source='short for "my parents"';
UPDATE vocab_item SET source='around (int... )' WHERE source='around, int...';
UPDATE vocab_item SET source='there, over there (-ì)' WHERE source='there/over there (-ì)';

UPDATE vocab_item SET source='sweater (6 letters)' WHERE source='sweater , 6 letters';
UPDATE vocab_item SET source='sweater (8 letters)' WHERE source='sweater , 8 letters';

UPDATE vocab_item SET source='better (adj)' WHERE source='better -adj';
UPDATE vocab_item SET source='not even (...o)' WHERE source='not even -...o';
UPDATE vocab_item SET source='not even (...e)' WHERE source='not even -...e';
UPDATE vocab_item SET source='nothing (...e)' WHERE source='nothing -...e';
UPDATE vocab_item SET source='single (for a woman)' WHERE source='single -f.';

UPDATE vocab_item SET active=false
  WHERE target='nuora'
    AND source='daughter in law'
    AND structure->>'vocabcoach-category'='noun1';

INSERT INTO vocab_item (target,source,structure,target_language,source_language)
  VALUES ('nuora','daughter in law','{"vocabcoach-category":"nounsingf"}'::json,'it','en');

INSERT INTO vocab_item (target,source,structure,target_language,source_language)
  VALUES ('nuore','daughters in law','{"vocabcoach-category":"nounplurf"}'::json,'it','en');

UPDATE vocab_item SET source='or (6 letters)' WHERE source='or -6 letters';
UPDATE vocab_item SET source='observer (male)' WHERE source='(male) observer';
UPDATE vocab_item SET source='observer (fem.)' WHERE source='(fem.) observer';
UPDATE vocab_item SET source='please (2 words -ia)' WHERE source='please (-ia)';
UPDATE vocab_item SET active=false WHERE source='very bad' AND structure->>'vocabcoach-category'='superlative';
UPDATE vocab_item SET source='very bad (p...)' WHERE source='very bad - p';

UPDATE vocab_item SET source='painter (masc.)' WHERE source='painter, m.';
UPDATE vocab_item SET source='painter (femm.)' WHERE source='painter, f.';

UPDATE vocab_item SET structure='{"vocabcoach-category":"adjinv"}'::json WHERE structure->>'vocabcoach-category'='adjinv--adjinv';

UPDATE vocab_item SET source='also, too (p...)' WHERE source='also, too, p...';
UPDATE vocab_item SET source='here (...-a)' WHERE source='here, ...-a';

UPDATE vocab_item SET source='this (fem.)' WHERE source='this, fem.';
UPDATE vocab_item SET source='these (fem.)' WHERE source='these, fem.';
UPDATE vocab_item SET source='this (masc.)' WHERE source='this, masc.';
UPDATE vocab_item SET source='these (masc.)' WHERE source='these, masc.';

UPDATE vocab_item SET source='here (...-i)' WHERE source='here, ...-i';
UPDATE vocab_item SET source='to shave (r-...)' WHERE source='to shave (r-)';
UPDATE vocab_item SET source='writer (masc.)' WHERE source='writer, masc.';
UPDATE vocab_item SET source='writer (fem.)' WHERE source='writer, fem.';

UPDATE vocab_item SET source='sculptor (m.)' WHERE source='sculptor, m.';
UPDATE vocab_item SET source='sculptor (f.)' WHERE source='sculptor, f.';

UPDATE vocab_item SET source='safe, sure' WHERE source='safe/sure';
UPDATE vocab_item SET source='only (4 letters)' WHERE source='only -4 letters';
UPDATE vocab_item SET source='under, below, underneath' WHERE source='under/below/underneath';
UPDATE vocab_item SET source='to spread (food, not rumors)' WHERE source='to spread (food, not rumors)';

UPDATE vocab_item SET active=false WHERE structure->>'vocabcoach-category'='father-in-law';
INSERT INTO vocab_item (target,source,structure,target_language,source_language)
  VALUES ('suocera','mother in law','{"vocabcoach-category":"nounsingf"}'::json,'it','en');

INSERT INTO vocab_item (target,source,structure,target_language,source_language)
  VALUES ('suocere','mothers in law','{"vocabcoach-category":"nounplurf"}'::json,'it','en');

INSERT INTO vocab_item (target,source,structure,target_language,source_language)
  VALUES ('suocero','father in law','{"vocabcoach-category":"nounsingm"}'::json,'it','en');

INSERT INTO vocab_item (target,source,structure,target_language,source_language)
  VALUES ('suoceri','fathers in law','{"vocabcoach-category":"nounplurm"}'::json,'it','en');

END TRANSACTION;
