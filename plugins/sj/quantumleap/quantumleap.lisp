;;;; PigIron CYCo sj config quantumleap quantumleap
;;;;

(instrument quantumleap
	    :parent +root-instrument+
	    :transient nil)

(instrument ql-bass
	    :parent quantumleap
	    :channel (meta-channel :bass)
	    :transient nil)

(instrument ql-guitar
	    :parent quantumleap
	    :channel (meta-channel :guitar)
	    :transient nil)

(instrument ql-gypsy
	    :parent quantumleap
	    :channel (meta-channel :gypsy)
	    :transient nil)

(instrument ql-vop
	    :parent quantumleap
	    :channel (meta-channel :vop)
	    :transient nil)

(instrument ql-drums
	    :parent quantumleap
	    :channel (meta-channel :drums)
	    :transient nil)
