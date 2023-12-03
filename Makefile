##
# gocix
#
# @file
# @version 0.1

# Introduction of the 'gocix' channel.
channel_intro_commit = cdb78996334c4f63304ecce224e95bb96bfd4c7d
channel_intro_signer = 8D10 60B9 6BB8 292E 829B  7249 AED4 1CC1 93B7 01E2

authenticate:
	echo "Authenticating Git checkout..." ;	\
	guix git authenticate					\
	    --cache-key=channels/guix --stats			\
	    "$(channel_intro_commit)" "$(channel_intro_signer)"

# end
