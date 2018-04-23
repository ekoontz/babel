echo "SELECT target,source,structure->>'vocabcoach-category' AS category FROM vocab_item WHERE active=true AND target_language='it' ORDER BY target ASC" | psql > vocab.txt
