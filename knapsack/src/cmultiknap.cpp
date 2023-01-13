#include <stdio.h>
#include <vector>
#include <algorithm>
#include <time.h>
#include <string.h>
#include <iostream>
#include <cstring>

#include <Rcpp.h>

using namespace std;
using namespace Rcpp;

/* Class used by local Improvement */
class GreedyObject {

    public:
	    double ratio;
	    int index;

    public:
   	    GreedyObject(){}
        GreedyObject(double r, int in) {
            ratio = r;
            index = in;
        }

        GreedyObject(const GreedyObject & copy) {
            this->ratio = copy.ratio;
            this->index = copy.index;
        }

        bool operator < (const GreedyObject &right) const {
            return this->ratio > right.ratio;
        }

	    bool operator > (const GreedyObject &right) const {
        	return this->ratio < right.ratio;
        }

        bool operator == (const GreedyObject &right) const {
        	return this->index == right.index;
        }

        bool operator != (const GreedyObject &right) const {
        	return this->index != right.index;
        }

        GreedyObject & operator = (const GreedyObject &copy) {
        	this->ratio = copy.ratio;
            this->index = copy.index;
            return *this;
        }
};

void print_str(char*);

int NUMBER_OBJECTS;     // populated automatically by processDataORLIB
int NUMBER_CONSTRAINTS;	// populated automatically by processDataORLIB
int OPTIMUM;	        // populated automatically by processDataORLIB
// int * CAPACITIES;       // populated automatically by processDataORLIB
// int ** CONSTRAINTS;     // populated automatically by processDataORLIB
// int * VALUES;           // populated automatically by processDataORLIB

const int POPULATION = 10; // size of the population
const int LOCAL_IMPROVEMENT = 10; // number of local improvements
const double INITIAL_POPULATION_PROB = 0.9; // the probability with which the initial population is generated
const int GENERATIONS = 100; // number of generations to run the algorithm
const int GREEDY_CROSSOVER = 1; // a constant to identify greedy crossover in adaptive crossover
const int ONE_POINT_CROSSOVER = 0; // a constant to identify one point crossover in adaptive crossover
const int NEGATIVE_FITNESS = -100; // the fitness of an invalid individual (violating constraints)
const int SHUFFLE_TOLERANCE = 100000; // the number of attempts to identify unique parents before shuffling
double MUTATION_PROBABILITY;	// populated in main()
double MUTATION_INCREMENT;	// populated in main()
double SHUFFLE_PROBABILITY;	// populated in main()
double MAX_MUTATION_PROBABILITY;	// populated in main()
int runtimeCrossoverType; // populated by adaptive crossover to identify which crossover operation was performed
const int MAX_UNIQUE_ITERATIONS = 10; // maximum iterations to spend on finding objects in local improvement

/* A list of objects sorted in non-increasing order of the lagrangian psuedo-utility ratio.
Used by localImprovement() */
vector<GreedyObject> GREEDY_OBJECTS;

vector<int> CAPACITIES(NUMBER_CONSTRAINTS);
vector<int> VALUES(NUMBER_OBJECTS);
vector< vector<int> > CONSTRAINTS(NUMBER_CONSTRAINTS);

/* Lagrangian Relaxation Paremeters */
const double INITIAL_LAMBDA    = 0;          // the initial value of lambdas
const double INITIAL_INCREMENT = 0.01;       // the value of the increment with which the lambdas are increased or decreased
const double LAMBDA_TOLERANCE  = 0.00000001; // the value of the increment at which the calculation terminates
const int COUNT_TOLERANCE      = 100;        // termination criteria constant for the inner loop of calculateLagrangianMultipliers
const int DIFF_TOLERANCE       = 2;          // termination criteria constant for the inner loop of calculateLagrangianMultipliers
vector<double> LAMBDAS;                      // the lagrangian multipliers - populated by calculateLagrangianMultipliers
vector<double> SOLUTION;                     // the lagrangian dual solution - used by calculateLagrangianMultipliers

/* Generate a random number in [min, max] */
int generateRandomNumber(int min, int max) {
    int r;
    double randd = ((double)rand() / ((double)(RAND_MAX)+(double)(1)));
    r = (int) (randd * (double)(max-min+1)) + min;
    return r;
}

/* Generate a random number in [0, 1) */
double generateDoubleRandomNumber() {
	return ((double)rand() / ((double)(RAND_MAX)+(double)(1)));
}

/* Return a random crossover type */
int randomCrossover() {
    int cc = (int)((double)generateDoubleRandomNumber() * (double)2);
	return cc;
}

/* Process the data from the ORLIB file */
void processDataORLIB(char * filename) {

	FILE * file;
	file = fopen(filename, "r");
    if(file == NULL) {
      printf("orlib1.txt File Not Found in Current Directory.");
      exit(1);
    }

	char * line = new char [1000];
	fgets(line, 1000, file);
	char * tok  = strtok(line, " ");
	NUMBER_OBJECTS = atoi(tok);
	tok = strtok(NULL, " ");
	NUMBER_CONSTRAINTS = atoi(tok);
	tok = strtok(NULL, " ");
	OPTIMUM = atoi(tok);

	int i = 0;

	/* VALUES (objective function) */
	// VALUES = new int[NUMBER_OBJECTS];
	fgets(line, 1000, file);
	tok = strtok(line, " ");

	while(true) {

        while(tok != NULL && i < NUMBER_OBJECTS) {
            int vv = atoi(tok);
            VALUES.push_back(vv);
            i++;
            tok = strtok(NULL, " ");
            if(tok == NULL) {
                break;
            }
            if(strcmp(tok, " ") == 0 || strcmp(tok, "\\n") == 0) {
                break;
            }
        }

		if(i < NUMBER_OBJECTS) {

			fgets(line, 1000, file);
			tok = strtok(line, " ");
			int vv = atoi(tok);
			VALUES.push_back(vv);
			tok = strtok(NULL, " ");
			i++;
		}
		else {
			break;
		}
	}

    /* CONSTRAINTS */
	// CONSTRAINTS = new int*[NUMBER_CONSTRAINTS];
	for(int n = 0; n < NUMBER_CONSTRAINTS; n++) {

		i=0;
		fgets(line, 1000, file);
		// CONSTRAINTS[n] = new int[NUMBER_OBJECTS];
		tok = strtok(line, " ");
		int vv = atoi(tok);
        vector<int> tmx;
		tmx.push_back(vv);
		i++;
		while(true) {
			tok = strtok(NULL, " ");
			while(tok != NULL && i < NUMBER_OBJECTS) {
				vv = atoi(tok);
				tmx.push_back(vv);
				tok = strtok(NULL, " ");
                i++;
                if(tok == NULL) {
                    break;
                }
                if(strcmp(tok, " ") == 0 || strcmp(tok, "\\n") == 0) {
                    break;
                }
		    }

			if(i < NUMBER_OBJECTS) {
				fgets(line, 1000, file);
				tok = strtok(line, " ");
				int vv = atoi(tok);
				tmx.push_back(vv);;
				i++;
			}
			else {
				break;
			}
		}
        CONSTRAINTS.push_back(tmx);
	}

	/* CAPACITIES */
	// CAPACITIES = new int[NUMBER_CONSTRAINTS];
	i=0;
	fgets(line, 1000, file);
	tok = strtok(line, " ");
	while(true) {

		while(tok != NULL && i < NUMBER_CONSTRAINTS) {
			int vv = atoi(tok);
			CAPACITIES.push_back(vv);
			i++;
			tok = strtok(NULL, " ");
            if(tok == NULL) {
                break;
            }
            if(strcmp(tok, " ") == 0 || strcmp(tok, "\\n") == 0) {
                break;
            }
		}

		if(i < NUMBER_CONSTRAINTS) {
			fgets(line, 1000, file);
			tok = strtok(line, " ");
			int vv = atoi(tok);
			CAPACITIES.push_back(vv);
			i++;
			tok = strtok(NULL, " ");
		}
		else {
			break;
		}
	}

  delete[](line);
}

/* Initialize lagrangian multipliers */
void initializeLagrangianMultipliers() {

    for (int i = 0; i < NUMBER_CONSTRAINTS; i++) {
        LAMBDAS.push_back(0.0);
    }
    for (int i = 0; i < NUMBER_OBJECTS; i++) {
        SOLUTION.push_back(0.0);
    }
}

/* Calculate the lagrangian dual solution */
vector<int> calculateSolution() {

	vector<double> solution(NUMBER_OBJECTS);
	vector<int>    knapsack(NUMBER_OBJECTS, 0);

    for (int i = 0; i < NUMBER_OBJECTS; i++)  {
        double w = 0;
        for (int j = 0; j < NUMBER_CONSTRAINTS; j++) {
            double ww = (double) CONSTRAINTS[j][i];
            ww *= LAMBDAS[j];
            w  += ww;
        }
        double v    = (double) VALUES[i] - w;
        solution[i] = v;
    }

    SOLUTION = solution;


    for (int i = 0; i < NUMBER_OBJECTS; i++) {
        if (solution[i] < 0) {
            knapsack[i] = 0;
        } else {
            knapsack[i] = 1;
        }
    }

    return knapsack;
}

/* Calculate the weights of this knapsack that is passed in */
vector<int> calculateWeights(vector<int> knapsack) {

    vector<int> weights(NUMBER_CONSTRAINTS, 0);

	for(int i = 0; i < NUMBER_CONSTRAINTS; i++) {
        for(int j = 0; j < NUMBER_OBJECTS; j++) {
            if(knapsack[j] == 1) {
                weights[i] += CONSTRAINTS[i][j];
            }
        }
	}

    return weights;
}

/* Calculate the value of the currently calculated Lagrangian Solution */
double calculateValue() {

	double value = 0;
    for(int i = 0; i < NUMBER_CONSTRAINTS; i++) {
        double ll = LAMBDAS[i] * (double) CAPACITIES[i];
        value += ll;
    }

    for(int i = 0; i < NUMBER_OBJECTS; i++) {
        if(SOLUTION[i] > 0) {
            value += SOLUTION[i];
        }
    }

    return value;
}

/* Calculate the lagrangian multipliers */
void calculateLagrangianMultipliers() {

    double increment = INITIAL_INCREMENT;
    double tolerance = LAMBDA_TOLERANCE;

    initializeLagrangianMultipliers();
  
    while (true) {

        int count = 0;
        double prevValue = -1;

        while (true) {

            vector<int> solution = calculateSolution();
            vector<int> weights  = calculateWeights(solution);
            bool   flag          = true;
            double value         = calculateValue();

            if (prevValue == -1) {
                prevValue = value;
            } else {
                double diff = prevValue - value;
                if (diff < 0) {
                    diff *= -1;
                }
                if (diff < DIFF_TOLERANCE) {
                    count++;
                    if(count >= COUNT_TOLERANCE) {
                        count = 0;
                        break;
                    }
                } else {
                    count = 0;
                }
                prevValue = value;
            }

            flag = true;
            for (int i = 0; i < NUMBER_CONSTRAINTS; i++) {
                if (weights[i] < CAPACITIES[i]) {
                    if (LAMBDAS[i] > 0) {
                        LAMBDAS[i]-=increment;
                        if(LAMBDAS[i] < 0) {
                            LAMBDAS[i] = 0;
                        }
                    }
                } else if (weights[i] > CAPACITIES[i]) {
                    flag = false;
                    LAMBDAS[i] += increment;
                }
            }
            if(flag) {
                break;
            }
        }
  
        if (increment <= tolerance) {
            break;
        }
        increment /= 2;
    }
}

/* A genome in the genetic algorithm */
class KNode {

    private:
	    int value;
	    vector<int> knapsack;
	    vector<int> weights;

    public:
	    int crossoverType;

        /* Constructor */
        KNode() {}
        /* Constructor */
        KNode(vector<int> knap) {

            /* Make a deep copy of the passed in array */
            for (int i = 0; i < NUMBER_OBJECTS; i++) {
                if (knap[i] == 1) {
                    knapsack.push_back(1);
                } else if(knap[i] == 0) {
                    knapsack.push_back(0);
                } else {
                    printf("Invalid Knapsack passed to constructor!");
                    printf("i = %d, value = %d", i, knap[i]);
                    exit(1);
                }
             }
             calculateWeights();
             calculateValue();
             crossoverType = randomCrossover();
        }
        /* Copy Constructor */
        KNode(const KNode & copy) {

            vector<int> knap = copy.knapsack;
            for(int i = 0; i < NUMBER_OBJECTS; i++) {
                if (knap[i] == 1) {
                    knapsack.push_back(1);
                } else if (knap[i] == 0) {
                    knapsack.push_back(0);
                } else {
                    printf("Invalid Knapsack passed to copy constructor!");
                    exit(1);
                }
            }
            crossoverType = copy.crossoverType;
            calculateWeights();
            calculateValue();
        }

        /* Destructor */
        ~KNode() {}

        /* Assignment Operator */
        const KNode & operator = (const KNode & copy) {

            vector<int> knap = copy.knapsack;
            for(int i = 0; i < NUMBER_OBJECTS; i++) {
                Rcout << copy.knapsack[i] << std::endl;
                // if(knap[i] == 1) {
                //     knapsack.push_back(1);
                // } else if(knap[i] == 0) {
                //     knapsack.push_back(0);
                // }
                // else {
                //     printf("Invalid Knapsack passed to copy constructor!");
                //     exit(1);
                // }
            }

            // crossoverType = copy.crossoverType;
            // calculateWeights();
            // calculateValue();
            return KNode();
        }


        bool operator < (const KNode &right) const {
            int af = this->nodeFitness();
            int bf = right.nodeFitness();
            return af > bf;
        }

        bool operator > (const KNode & right) const {
      	    int af = this->nodeFitness();
            int bf = right.nodeFitness();
            return af < bf;
        }

        bool operator == (const KNode & right) const {
      	    int af = this->nodeFitness();
            int bf = right.nodeFitness();
            return af == bf;
        }

        bool operator != (const KNode & right) const {
      	    int af = this->nodeValue();
            int bf = right.nodeValue();
            return af != bf;
        }


        /* Does this knapsack object violate any of the constraints? */
		bool violatesConstraints(void) const {
			for (int i = 0; i < NUMBER_CONSTRAINTS; i++) {
				if (this->weights[i] > CAPACITIES[i]) {
					return true;
				}
			}
			return false;
		}
        /* The fitness of this object - This is different from the value. If the
       	object violates any of the constraints, a negative value is returned */
		int nodeFitness() const {
		    if (violatesConstraints()) {
			    return NEGATIVE_FITNESS;
			}
		    return nodeValue();
		}

        /* Return the value of this knapsack object */
		int nodeValue() const {
            return value;
		}

        /* Calculate the weights of this knapsack object */
        void calculateWeights() {
			for (int i = 0; i < NUMBER_CONSTRAINTS; i++) {
				weights.push_back(0);
				for (int j = 0; j < NUMBER_OBJECTS; j++) {
					if (knapsack[j] == 1) {
						weights[i] += CONSTRAINTS[i][j];
					}
				}
            }
		}

        /* Calculate the value of this knapsack object */
     	void calculateValue() {
	        value = 0;
		    for(int i = 0; i < NUMBER_OBJECTS; i++) {
				if (knapsack[i] == 1) {
				    value += VALUES[i];
                }
            }
        }

        /* Replace the value at the passed in index to 1 */
		void addOne(int index) {
			if (knapsack[index] == 1) {
				return;
			}
			value+=VALUES[index];
			for (int i = 0; i < NUMBER_CONSTRAINTS; i++) {
				weights[i] += CONSTRAINTS[i][index];
			}
			knapsack[index] = 1;
		}

        /* Replace the value at the passed in index to 0 */
		void resetBit(int index)
		{
			if (knapsack[index] == 0) {
				return;
			}
			value -= VALUES[index];
			for (int i = 0; i < NUMBER_CONSTRAINTS; i++) {
				weights[i] -= CONSTRAINTS[i][index];
			}
			knapsack[index] = 0;
		}

        /* Verify that the object of this class is a valid knapsack */
        void checkKNode() const {
        	for(int i = 0; i < NUMBER_OBJECTS; i++) {
           	    if (knapsack[i] == 1 || knapsack[i] == 0) {
                } else {
                    // Should Never Happen
              	    printf("Bad Knapsack");
                    exit(1);
                }
            }
        }


        /* Set the value represented by this knapsack */
		void setValue(int v) {
			value = v;
		}

        /* Set the weights of this knapsack object */
		void setWeights(vector<int> w) {
			weights = w;
		}

        /* Set the array that represents the chosen and not chosen objects in order */
		void setKnapsack(vector<int> knap) {
			knapsack = knap;
		}

        /* Return the array that represents the chosen and not chosen objects in order */
        vector<int> getKnapsack() {
      	    return knapsack;
        }

        /* Is the object represented by the index chosen? If yes, return 1. If no, return 0 */
        int getValueOfIndex(int index) {
			return knapsack[index];
		}

        /* Return a clone of this object - All member variables are deep copied */
        KNode clone() {
            vector<int> kk;
            int ct = crossoverType;
            for(int i = 0; i < NUMBER_OBJECTS; i++) {
               kk.push_back(knapsack[i]);
            }

            KNode clone(kk);
            clone.crossoverType = ct;
            return clone;
        }
};

/* Generate a random initial population */
vector<KNode> generateRandomPopulation() {

	vector<KNode> population;
	for (int i = 0; i < POPULATION; i++) {
		vector<int> knapsack;
		for (int j = 0; j < NUMBER_OBJECTS; j++) {
			double rand = generateDoubleRandomNumber();
			if(rand < INITIAL_POPULATION_PROB) {
				knapsack.push_back(1);
			}
			else {
				knapsack.push_back(0);
			}
		}

		KNode node(knapsack);
		population.push_back(node);
	}

	return population;
}

/* Mutation */
inline void mutate(KNode &node, double prob) {

    vector<int> knapsack = node.getKnapsack();
	for (int i = 0; i < NUMBER_OBJECTS; i++) {
		double rand = generateDoubleRandomNumber();
		if (rand < prob) {
			// invert
			if (knapsack[i] == 1) {
				node.resetBit(i);
			}
			else if (knapsack[i] == 0) {
				node.addOne(i);
			}
			else {
         	    // Should never happen
         	    printf("%d\n\n", knapsack[i]);
				printf("Invalid Knapsack");
				exit(1);
			}
		}
	}
}


/* Shuffler */
inline void shufflePopulation(vector<KNode> &population) {

	printf("\nShuffling...\n");
	for(int i=0; i<POPULATION; i++) {
		KNode node = population[i];
		mutate(node, SHUFFLE_PROBABILITY);
	}
}

/* Random Selection */
inline vector<KNode> randomSelection(vector<KNode> &population) {

    vector<KNode> parents;
	int count = 0;
    KNode node1;
    KNode node2;

    while (1) {

        int rand1 = generateRandomNumber(0, POPULATION-1);
		int rand2 = generateRandomNumber(0, POPULATION-1);
		if (rand1 == rand2)
		    continue;

        node1.~KNode();
        node2.~KNode();
		node1 = population[rand1];
		node2 = population[rand2];
		//if(node1.nodeValue() != node2.nodeValue())
		{
      	    parents.push_back(node1);
            parents.push_back(node2);
			break;
		}
		count++;
		if (count > SHUFFLE_TOLERANCE) {
		    shufflePopulation(population);
		}
	}

	return parents;
}

/* One Point Crossover */
inline vector<KNode> onePointCrossover(KNode &n1, KNode &n2) {

	int rand = generateRandomNumber(1, NUMBER_OBJECTS-1);
	vector<int> knap1;
	vector<int> knap2;
	int i = 0;

	for(i=0; i<rand; i++) {
		int v1 = n1.getValueOfIndex(i);
		int v2 = n2.getValueOfIndex(i);
		knap1.push_back(v1);
		knap2.push_back(v2);
	}

	for(i=rand; i<NUMBER_OBJECTS; i++) {
		int v1 = n2.getValueOfIndex(i);
		int v2 = n1.getValueOfIndex(i);
		knap1.push_back(v1);
		knap2.push_back(v2);
	}

	vector<KNode> children;

	KNode node1(knap1);
    KNode node2(knap2);

    /* Try to add items till the knapsack node1 is filled */
    for(int i=0; i<NUMBER_OBJECTS; i++) {
   	    GreedyObject obj = GREEDY_OBJECTS[i];
        int ind = obj.index;
        if(node1.getValueOfIndex(ind) == 0) {
          node1.addOne(ind);
        }

        if(node1.violatesConstraints()) {
            node1.resetBit(ind);
        }
    }

     /* Try to add items till the knapsack node2 is filled */
     for (int i = 0; i < NUMBER_OBJECTS; i++) {
         GreedyObject obj = GREEDY_OBJECTS[i];
         int ind = obj.index;
         if (node2.getValueOfIndex(ind) == 0) {
     	       node2.addOne(ind);
         }
         if (node2.violatesConstraints()) {
               node2.resetBit(ind);
         }
     }

     children.push_back(node1);
     children.push_back(node2);
     return children;
}

/* Compare Object for sorting - Used by greedyCrossover */
class CompareObject {

    public:

	    double ratio;
		int index;

		CompareObject() {
			ratio = 0;
			index = 0;
		}

		~CompareObject() {}

		CompareObject(const CompareObject &m) {
			ratio = m.ratio;
			index = m.index;
		}

		const CompareObject& operator =(const CompareObject &right) {
			ratio = right.ratio;
			index = right.index;
			return right;
		}
};

/* Function for qsort */
int compareObjects(const void *a, const void * b) {

	CompareObject o1 = *((CompareObject*)a);
	CompareObject o2 = *((CompareObject*)b);

	if (o1.ratio < o2.ratio) {
		return 1;
	}
	else if (o1.ratio > o2.ratio) {
		return -1;
	}
	else {
		return 0;
	}
}

/* Greedy Crossover */
inline vector<KNode> greedyCrossover(KNode &n1, KNode &n2) {

	vector<CompareObject> vec;
	int i=0;
	int count = 0;
	for (i=0; i<NUMBER_OBJECTS; i++) {

		if (n1.getValueOfIndex(i)==1) {
			double ww = 0;
			for (int j = 0; j < NUMBER_CONSTRAINTS; j++) {
                double www = (double)CONSTRAINTS[j][i] * LAMBDAS[j];
				ww +=www;
			}
            if(ww == 0) {
         	    ww = 1;
            }

			double ratio = (double)VALUES[i] / ww;
			CompareObject obj;
			obj.index = i;
			obj.ratio = ratio;

			vec.push_back(obj);

			count++;
        } else if (n2.getValueOfIndex(i)==1) {

			double ww = 0;
			for (int j = 0; j < NUMBER_CONSTRAINTS; j++) {
				ww += (double)CONSTRAINTS[j][i];
			}

			double ratio = (double)VALUES[i] / ww;
			CompareObject obj;
			obj.index = i;
			obj.ratio = ratio;

			vec.push_back(obj);

			count++;
		}
	}

	CompareObject * arr = new CompareObject[count];

	for(i = 0; i < count; i++) {
		arr[i] = vec[i];
	}

	qsort(arr, count, sizeof(CompareObject), compareObjects);

	vector<int> kk;
	for (i = 0; i < NUMBER_OBJECTS; i++) {
		kk.push_back(0);
	}

	KNode node(kk);

	for (i = 0; i < count; i++) {
		CompareObject obj = arr[i];
		int index = obj.index;
		node.addOne(index);
		if(node.violatesConstraints()) {
			node.resetBit(index);
		}
	}

   /* Try to add items till the knapsack is filled */
   for(int i=0; i<NUMBER_OBJECTS; i++) {
   	GreedyObject obj = GREEDY_OBJECTS[i];
      int ind = obj.index;
      if(node.getValueOfIndex(ind) == 0) {
   		node.addOne(ind);
      }

      if(node.violatesConstraints()) {
      	node.resetBit(ind);
      }
   }

	delete [] arr;
    vec.erase(vec.begin(), vec.end());

	vector<KNode> childd;
	childd.push_back(node);
	return childd;
}

/* Adaptive Crossover */
vector<KNode> crossover(KNode &node1, KNode &node2) {

	int type = -1;
	if(node1.crossoverType == node2.crossoverType) {
		type = node1.crossoverType;
		runtimeCrossoverType = type;
	} else {
		type = randomCrossover();
		runtimeCrossoverType = type;
	}

	if(type == ONE_POINT_CROSSOVER) {
		vector<KNode> children = onePointCrossover(node1, node2);

		if(children[0].nodeFitness() > node1.nodeFitness()) {
			// successful crossover
			children[0].crossoverType = ONE_POINT_CROSSOVER;
			node1.crossoverType = ONE_POINT_CROSSOVER;
		} else {
			children[0].crossoverType = randomCrossover();
			node1.crossoverType = randomCrossover();
		}

		if(children[0].nodeFitness() > node2.nodeFitness()) {
			// successful crossover
			children[0].crossoverType = ONE_POINT_CROSSOVER;
			node2.crossoverType = ONE_POINT_CROSSOVER;
		}
		else {
			children[0].crossoverType = randomCrossover();
			node2.crossoverType = randomCrossover();
		}

		if(children[1].nodeFitness() > node1.nodeFitness()) {
			// successful crossover
			children[1].crossoverType = ONE_POINT_CROSSOVER;
			node1.crossoverType = ONE_POINT_CROSSOVER;
		}
		else {
			children[1].crossoverType = randomCrossover();
			node1.crossoverType = randomCrossover();
		}

		if(children[1].nodeFitness() > node2.nodeFitness()) {
			// successful crossover
			children[1].crossoverType = ONE_POINT_CROSSOVER;
			node2.crossoverType = ONE_POINT_CROSSOVER;
		}
		else {
			children[1].crossoverType = randomCrossover();
			node2.crossoverType = randomCrossover();
		}

		return children;
	} else if(type == GREEDY_CROSSOVER) {
		vector<KNode> children = greedyCrossover(node1, node2);

		if(children[0].nodeFitness() > node1.nodeFitness()) {
			// successful crossover
			children[0].crossoverType = GREEDY_CROSSOVER;
			node1.crossoverType = GREEDY_CROSSOVER;
		} else {
			children[0].crossoverType = randomCrossover();
			node1.crossoverType = randomCrossover();
		}

		if(children[0].nodeFitness() > node2.nodeFitness()) {
			// successful crossover
			children[0].crossoverType = GREEDY_CROSSOVER;
			node2.crossoverType = GREEDY_CROSSOVER;
		} else {
			children[0].crossoverType = randomCrossover();
			node2.crossoverType = randomCrossover();
		}

		return children;
	} else {
   	    // Should never happen
		printf("Could Not Identify Crossover Type %d", type);
		exit(1);
	}

}

/* Try to randomly improve upon the passed in KNode, around the greedy neighborhood */
inline void localImprovement(KNode &n) {

	if(n.violatesConstraints())
   	return;

	KNode best(n);
    KNode node(n);
    KNode prev(n);
    for (int i=0; i<LOCAL_IMPROVEMENT; i++) {

        int count = 0;
      	int max = 0;
   	    while(true) {

      	int rand = generateRandomNumber(0, (NUMBER_OBJECTS-1));
        max++;
        if(max > MAX_UNIQUE_ITERATIONS) {
        	break;
        }

        if(node.getValueOfIndex(rand) == 1) {
            node.resetBit(rand);
            count++;
        }

        if(count == 5)
        	break;
        }

        for (int i = 0; i < NUMBER_OBJECTS; i++) {
       	    GreedyObject obj = GREEDY_OBJECTS[i];
            int index = obj.index;
            node.addOne(index);
            if(node.violatesConstraints()) {
         	    node.resetBit(index);
            }
        }

        if(node.nodeFitness() > best.nodeFitness()) {
            best.~KNode();
			best = node;
            prev.~KNode();
            prev = node;
      } else {
   		node.~KNode();
         node = prev;
      }
    }

    KNode ret(best.getKnapsack());
    n.~KNode();
	n = ret;
}

/* Greedy Algorithm - Creates a vector of objects arranged in non-increasing order of the
psuedu utility ratio. This vector is used by the local improvement function */
void greedyAlgorithm(void) {

	for (int i = 0; i < NUMBER_OBJECTS; i++) {
        int value = VALUES[i];
        double ww = 0;
        for (int j = 0; j < NUMBER_CONSTRAINTS; j++) {
      	    double www = LAMBDAS[j] * CONSTRAINTS[j][i];
            ww+=www;
        }
        if(ww == 0) {
      	    ww = 1;
        }

        double ratio = (double)value / ww;
        GreedyObject obj(ratio, i);
        GREEDY_OBJECTS.push_back(obj);
    }

    sort(GREEDY_OBJECTS.begin(), GREEDY_OBJECTS.end());
}


// [[Rcpp::export]]
Rcpp::List rProcessDataORLIB(string filename) {
    char *filx = new char[filename.length() + 1];
    strcpy(filx, filename.c_str());
  
    processDataORLIB(filx);
  
    delete[](filx);
  
    return Rcpp::List::create(
        Rcpp::Named("values")          = Rcpp::wrap(VALUES),
        Rcpp::Named("capacities")      = Rcpp::wrap(CAPACITIES),
        Rcpp::Named("constraints")     = Rcpp::wrap(CONSTRAINTS),
        Rcpp::Named("num_objects")     = NUMBER_OBJECTS,
        Rcpp::Named("num_constraints") = NUMBER_CONSTRAINTS,
        Rcpp::Named("optimum")         = OPTIMUM
    );
}

// [[Rcpp::export]]
Rcpp::List rGenTest(string filename) {
    char *filx = new char[filename.length() + 1];
    strcpy(filx, filename.c_str());

    processDataORLIB(filx);
    calculateLagrangianMultipliers();

    /* Set the mutation probabilities */
	SHUFFLE_PROBABILITY = (double)10 / (double)NUMBER_OBJECTS;
	MUTATION_PROBABILITY = (double)1 / (double)NUMBER_OBJECTS;
	MUTATION_INCREMENT = (double)1 / (double)NUMBER_OBJECTS;
	MAX_MUTATION_PROBABILITY = (double)10 / (double)NUMBER_OBJECTS;

    vector<KNode> population;
    population.reserve(POPULATION);
	population = generateRandomPopulation();

    int i=0;
    /* Create a global vector GREEDY_OBJECTS for use in the local improvement procedure */
    greedyAlgorithm();

    /* Declaration of variables used for adaptive mutation rate change */
	KNode gBest;
	KNode prevBest;
    for (i = 0; i < GENERATIONS; i++) {
        /* The genetic algorithm is generational. All offspring replace their parents.
        newPopulation is populated with the offspring */
	    vector<KNode> newPopulation;
        int n = 0;
		int count = POPULATION;
		sort(population.begin(), population.end());

        /* Elitism - The best individual of the previous population
      	is copied to the next generation */
        newPopulation.push_back(population[0].clone()); // should this be population[0].clone()?
        gBest.~KNode();
	    gBest = population[0];
        // printf("%d:%d\n", i, gBest.nodeFitness());
        // if(gBest.nodeFitness() == OPTIMUM) {
		//     printf("Optimum Found!\n");
		//     break;
		// }
        // if(i==0) {
		//     prevBest = gBest;
		// }
		// else
		// {
        //     /* Update mutation probabilities */
		// 	if (prevBest.nodeFitness() == gBest.nodeFitness()) {
		// 		if (MUTATION_PROBABILITY < MAX_MUTATION_PROBABILITY) {
		// 			MUTATION_PROBABILITY += MUTATION_INCREMENT;
		// 		}
		// 	}
		// 	else {
		// 		MUTATION_PROBABILITY = MUTATION_INCREMENT;
		// 	}
		// }
        // prevBest.~KNode();
		// prevBest = gBest;

		// count--;
        // for (int m = 0; m < count;) {
        //     /* Randomly select parents for crossover */
		// 	vector<KNode> parents = randomSelection(population);

        //     /* Call adaptive crossover */
		//     vector<KNode> children = crossover(parents[0], parents[1]);

        //     /* Mutation and Local Improvement */
        //     if (runtimeCrossoverType == GREEDY_CROSSOVER) {

		// 		m += 1;
		// 		if (children[0].nodeFitness() <= parents[0].nodeFitness() && children[0].nodeFitness() <= parents[1].nodeFitness()) {
		// 			mutate(children[0],MUTATION_PROBABILITY);
		// 		}
        //         localImprovement(children[0]);
		// 		newPopulation.push_back(children[0]);
		// 		n++;

		// 	}
		// 	else if (runtimeCrossoverType == ONE_POINT_CROSSOVER) {

		// 		m += 2;
		// 		if (children[0].nodeFitness() <= parents[0].nodeFitness() && children[0].nodeFitness() <= parents[1].nodeFitness()) {
		// 			mutate(children[0],MUTATION_PROBABILITY);
		// 		}

		// 		if (children[1].nodeFitness() <= parents[0].nodeFitness() && children[1].nodeFitness() <= parents[1].nodeFitness()) {
		// 			mutate(children[1], MUTATION_PROBABILITY);
		// 		}

        //         localImprovement(children[0]);
		// 		newPopulation.push_back(children[0]);
		// 		n++;
        //         localImprovement(children[1]);
		// 		newPopulation.push_back(children[1]);
		// 		n++;
		// 	}
		// 	else {
        //  	    // Should Never Happen
		// 	    printf("Unknown runtime Crossover Type");
		// 		exit(1);
		// 	}
        // }
        /* Generational Replacement */
		// population.swap(newPopulation);
        // newPopulation.erase(newPopulation.begin(), newPopulation.end());
    }
    printf("Solution Objective Function: %d\n", gBest.nodeFitness());
    printf("Objects chosen:\n");

    // for (int i=0; i<NUMBER_OBJECTS; i++) {
    //     if (gBest.getValueOfIndex(i) == 1) {
    //         printf("%d ", i);
    //     }
    // }
    // printf("\n\n");

    // population.erase(population.begin(), population.end());
    // GREEDY_OBJECTS.erase(GREEDY_OBJECTS.begin(), GREEDY_OBJECTS.end());


    delete[](filx);

    return Rcpp::List::create(
        Rcpp::Named("lambdas")  = Rcpp::wrap(LAMBDAS),
        Rcpp::Named("solution") = Rcpp::wrap(SOLUTION)
    );
}

// [[Rcpp::export]]
Rcpp::List vv2rlistTest() {
    vector< vector<int> > v{ { 1, 2, 3 }, 
                             { 4, 5, 6 }, 
                             { 7, 8, 9 } };
    return Rcpp::wrap(v);
}

void print_str(char* strx) {
    for(int i = 0; i < strlen(strx); i++) {
  		cout << strx[i]; 
  	}
  	cout << endl;
}

