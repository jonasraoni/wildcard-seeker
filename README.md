# Wildcard Seeker

A wildcard searching/replacing class written on Pascal and optimized for speed.

It supports searching for line-breaks, case insensitive, whole words, backward/forward search, wildcards, how many characters should be kept as left/right "context" and the line break type.
For each match, it has information of the position where it was found, column, line, left/right context text and text replacement.

Due to lack of knowledge (or maybe too much preciosism), it was inlined manually by setting up 4 different search functions for specific cases (SearchBackwardWithWildCard, SearchBackward, SearchForwardWithWildCard, SearchForward), one of them is chosen and assigned to the search property, which is a delegate type.
