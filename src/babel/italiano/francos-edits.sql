UPDATE vocab_item SET source='provided that (condition)' WHERE source = 'provided that -condition-' AND active=true;
UPDATE vocab_item SET source='now (a...)' WHERE source = 'now - a...' AND active=true;

