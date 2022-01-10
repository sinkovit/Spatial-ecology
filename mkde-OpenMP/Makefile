# For Intel compilers
CXX         = icpc
OBJS        = ParallelMKDE01.o
CXXFLAGS    = -O3 -xHOST -openmp -mkl

# For GCC compilers
# Need 4.6 or later for AVX support (-mavx)
#CXX         = g++
#OBJS        = ParallelMKDE01.o
#CXXFLAGS    = -O3 -mavx -fopenmp

all: ParallelMKDE

ParallelMKDE: $(OBJS) ParallelMKDE01.h Makefile
	$(CXX) $(CXXFLAGS) -o $@ $(OBJS)

clean:
	@rm *.o *.oo ParallelMKDE
