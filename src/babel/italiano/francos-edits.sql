UPDATE vocab_item SET source='provided that (condition)' WHERE source = 'provided that -condition-' AND active=true;
UPDATE vocab_item SET source='now (a...)' WHERE source = 'now - a...' AND active=true;
UPDATE vocab_item SET source='so that' WHERE target='affinché' AND active=true;
DELETE FROM vocab_item WHERE target='basso' AND source='short -vs tall' AND active=true;
UPDATE vocab_item SET source='forest, words (sing.)' WHERE source='forest,woods' AND active=true;
UPDATE vocab_item SET source='to sparkle, shine' WHERE source='to sparkle,shine' AND active=true;
UPDATE vocab_item SET source='caraffe, pitcher' WHERE source='caraffe,pitcher' AND active=true;
UPDATE vocab_item SET source='cascade, waterfall' WHERE source='cascade,waterfall' AND active=true;
UPDATE vocab_item SET source='single (for a man)' WHERE source='male-single' AND active=true;
