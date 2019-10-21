# Habrahabr reader

Telegram bot for filtering habrahabr articles. It can help to get rid of useless articles and concentrate on important ones.

Criteria: article rating + author rating + mean of tags ratings >= threshold.

Each user can define his personal threshold and ratings for each author or tag.

Negative values like -1000 may completely ban author or tag.
Bot saves it's own state in json in 'saves' dir.

Work in progress, bugs are possible.
