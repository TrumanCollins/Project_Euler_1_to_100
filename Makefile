# Use the first for building debug, the second for optimized.
# Use -pg to get profiling date for gprof.

# Use the first to build euler_h for performance data, the second for
# single-threaded execution, and the multi for multi-threading.

#EULER_H_OPT = -O2 -dynamic -Wall -XBangPatterns -freverse-errors
EULER_H_OPT = -O2 -Wall -XBangPatterns -freverse-errors

euler_h : euler_h.hs EulerUtil.o PrimeMod.o Euler1to100.o
	ghc $(EULER_H_OPT) euler_h.hs

EulerUtil.o : EulerUtil.hs PrimeMod.o
	ghc $(EULER_H_OPT) EulerUtil.hs

Euler1to100.o : Euler1to100.hs EulerUtil.o PrimeMod.o
	ghc $(EULER_H_OPT) -fno-warn-unused-binds Euler1to100.hs

PrimeMod.o : PrimeMod.hs
	ghc $(EULER_H_OPT) PrimeMod.hs

clean :
	rm *.o *~ *.hi; echo
