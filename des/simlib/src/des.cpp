#include <stdlib.h>
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::plugins("cpp17")]]

struct master {
    double *data;
    master *next;
    master *prev;
};

class DES {

    private:

        static const int eTime_=1, eType_=2; // stackoverflow.com/a/3531105
        static const int svar_size_=26, tvar_size_=51, tim_var_=25;

        master **head;
        master **tail;
        int    *list_size, *list_rank;
        int maxlist_, maxatr_, elidx_; // edidx_ is the index of the event list 
        int next_event_type_;
        double simtime_;
        // Stores insert/remove arrays
        // When used to insert/remove events: indexes 1 and 2 are reserved for event time and event type.
        std::vector<double> transfer;

        void freemem_() {
            for (int i = 0; i < maxlist_; i++) {
                if (list_size[i] > 0) {
                    free(head[i]);
                    free(tail[i]);
                }
            }
            free(head);
            free(tail);
            free(list_size);
            free(list_rank);
        }

        void init_list(int i, master *row) {
            head[i]   = row;
            tail[i]   = row;
            row->next = NULL;
            row->prev = NULL;
        }

        void insert_first_(int i, master *row) {
            master *ihead = head[i];
            ihead->prev   = row;
            row->next     = ihead;
            row->prev     = NULL;
            head[i]       = row;
        }

        void insert_last_(int i, master *row) {
            master *itail = tail[i];
            row->prev     = itail;
            itail->next   = row;
            row->next     = NULL;
            tail[i]       = row;
        }

        void copy_from_transfer_(master *row) {
            // Copy the row values from the transfer array
            long n = std::min(transfer.size()-1,(unsigned long)maxatr_); // truncation of transfer
            row->data = (double*) calloc (maxatr_+1, sizeof(double));
            for (int j = 0; j <= n; j++) {
                row->data[j] = transfer[j];
            }
        }

        // TEST ONLY
        void copyvec(int i, std::vector<double> &x) {
            master *row = (master *) malloc (sizeof(master));
            if (++list_size[i] == 1) {
                init_list(i, row);
            }
            row = head[i];
            row->data = (double*) calloc (maxatr_+1, sizeof(double));
            int j = 0;
            for (auto xj : x) {
                row->data[j++] = xj;
            }
        }

    public:

        DES(int maxlist, int maxatr) {
            simtime_ = 0.0;
            maxlist_ = (maxlist < 1) ? 25 : maxlist;
            elidx_   = maxlist_; // Default index for event list
            maxatr_  = (maxatr  < 4) ? 10 : maxatr;
            // Pre-allocate memory space
            int listsize = maxlist_ + 1;
            list_size = (int*)     calloc (listsize,  sizeof(int));
            list_rank = (int*)     calloc (listsize,  sizeof(int));
            head      = (master**) calloc (listsize,  sizeof(master*));
            tail      = (master**) calloc (listsize,  sizeof(master*));
            for (int i = 1; i <= maxlist_; i++) { // List index 0 is not used
                head[i] = NULL;
                tail[i] = NULL;
                list_size[i] = 0;
                list_rank[i] = 0;
            }
            list_rank[elidx_] = eTime_; // Event list is ranked by event time
            transfer.reserve(maxatr_+1);
            // Initialize accumulators
            sampst(0.0,0);
            timest(0.0,0);
        }
        DES(int maxlist) : DES(maxlist, 10) {}
        DES() : DES(25, 10) {}
        ~DES() {}

        void freemem() { freemem_(); }

        double get_simtime()                  { return this->simtime_;          }
        void   set_simtime(double t)          { this->simtime_= t;              }
        int    get_maxlist()                  { return this->maxlist_;          }
        int    get_maxatr()                   { return this->maxatr_;           }
        int    get_next_event_type()          { return this->next_event_type_;  }
        void   set_next_event_type(int type)  { this->next_event_type_ = type;  }
        int    get_listsize(int i)            { return this->list_size[i];      }

        double get_transfer(int key) {
            return this->transfer[key];
        }

        void set_transfer(int key, double value) {
            this->transfer[key] = value;
        }

        void timing() {
            // Remove first entry from event list (indexed by elidx_)
            list_remove(elidx_,true);
            if (this->transfer[eTime_] < simtime_) { // Index 1 is the event time.
                exit(1);
            }
            // Advance the simulation clock and set the next event type
            this->simtime_         = this->transfer[eTime_];
            this->next_event_type_ = this->transfer[eType_];
        }

        void event_schedule(double time_of_event, int type_of_event) {
            transfer[eTime_] = time_of_event;
            transfer[eType_] = type_of_event;
            list_insert(elidx_,true);
        }

        int event_cancel(int event_type) {
            // Remove the first event of type event_type from the event list (indexed by elidx_)
            master *row, *ahead, *behind;
            static double high, low, value;

            if (list_size[elidx_] == 0) {
                return 0;
            }
            // Search the event list
            row   = head[elidx_];
            low   = event_type - 0.01;
            high  = event_type + 0.01;
            value = row->data[eType_];
            while (((value <= low) || (value >= high)) && (row != tail[elidx_])) {
                row   = row->next;
                value = row->data[eType_];
            }
            // Check if this is the end of the event list
            if (row == tail[elidx_]) {
                if ((value > low) && (value < high)) {
                    list_remove(elidx_,false); // remove last 
                    return 1;
                } else {
                    return 0;
                }
            }
            // Check if this is the head of the list
            if (row == head[elidx_]) {
                list_remove(elidx_,true); // remove first
                return 1;
            }
            // Else remove the event from the interior of the event list
            ahead        = row->next;
            behind       = row->prev;
            behind->next = ahead;
            ahead->prev  = behind;
            list_size[elidx_]--;
            // Copy and free memory
            /* transfer = std::vector<double>(row->data, row->data+maxatr_); // mem leakage */
            for (int j = 0; j <= maxatr_; j++) {
                this->transfer[j] = row->data[j];
            }
            free(row);
            /* Update the area under the number-in-list curve. */
            timest((double)list_size[elidx_], tim_var_ + elidx_);
            return 1;
        }

        void list_insert(int i, bool ascending) {
            master *row, *ahead, *behind;
            int    j;
            bool   post;
            if (++list_size[i]==1) {
                row = (master *) malloc (sizeof(master));
                init_list(i, row);
            } else {
                j      = list_rank[i];
                row    = head[i];
                behind = NULL;
                if (ascending) {
                    post = (transfer[j] >= row->data[j]);
                    while (post) {
                        behind = row;
                        row    = row->next;
                        post   = (behind != tail[i]);
                        if (post) {
                            post = (transfer[j] >= row->data[j]);
                        }
                    }
                } else {
                    post = (transfer[j] <= row->data[j]);
                    while(post) {
                        behind = row;
                        row    = row->next;
                        post   = (behind != tail[i]);
                        if (post) {
                            post = (transfer[j] <= row->data[j]);
                        }
                    }
                }
                if (row == head[i]) {
                    row = (master *) malloc (sizeof(master));
                    insert_first_(i, row);
                } else if (behind == tail[i]) {
                    row = (master *) malloc (sizeof(master));
                    insert_last_(i, row);
                } else {
                    row          = (master *) malloc (sizeof(master));
                    ahead        = behind->next;
                    row->prev    = behind;
                    behind->next = row;
                    ahead->prev  = row;
                    row->next    = ahead;
                }
            
            }
            copy_from_transfer_(row);
            /* Update the area under the number-in-list curve. */
            timest((double)list_size[i], tim_var_ + i);
        }

        void list_insert_first(int i) {
            master *row = (master *) malloc (sizeof(master));
            if (++list_size[i]==1) {
                init_list(i, row);
            } else {
                insert_first_(i, row);
            }
            copy_from_transfer_(row);
            /* Update the area under the number-in-list curve. */
            timest((double)list_size[i], tim_var_ + i);
        }

        void list_insert_last(int i) {
            master *row = (master *) malloc (sizeof(master));
            if (++list_size[i]==1) {
                init_list(i, row);
            } else {
                insert_last_(i, row);
            }
            copy_from_transfer_(row);
            /* Update the area under the number-in-list curve. */
            timest((double)list_size[i], tim_var_ + i);
        }

        void list_remove(int i, bool first) {
            master *row, *ihead, *itail;
            if (--list_size[i] == 0) {
                row     = head[i];
                head[i] = NULL;
                tail[i] = NULL;
            } else {
                if (first) {
                    row         = head[i];
                    ihead       = row->next;
                    ihead->prev = NULL;
                    head[i]     = ihead;
                } else {
                    row         = tail[i];
                    itail       = row->prev;
                    itail->next = NULL;
                    tail[i]     = itail;
                }
            }
            // Copy and free memory
            // this->transfer = std::vector<double>(row->data, row->data+maxatr_); // mem leakage
            for (int j = 0; j <= maxatr_; j++) {
                this->transfer[j] = row->data[j];
            }
            free(row);
            /* Update the area under the number-in-list curve. */
            timest((double)list_size[i], tim_var_ + i);
        }

        double sampst(double value, int varidx) {

        /* Initialize, update, or report statistics on discrete-time processes: 
         * sum/average, max (default -1E30), min (default 1E30), number of observations 
         * for sampst variable "variable", where "variable":
         * = 0 initializes accumulators
         * > 0 updates sum, count, min, and max accumulators with new observation
         * < 0 reports stats on variable "variable" and returns them in transfer:
         *      [1] = average of observations
         *      [2] = number of observations
         *      [3] = maximum of observations
         *      [4] = minimum of observations */

            static int ivar, num_obs[svar_size_];
            static double max[svar_size_], min[svar_size_], sum[svar_size_];

            if (varidx > 0) {
                sum[varidx] += value;
                if (value > max[varidx]) {
                    max[varidx] = value;
                }
                if (value < min[varidx]) {
                    min[varidx] = value;
                }
                num_obs[varidx]++;
                return 0.0;
            }
            if (varidx < 0) {
                ivar = -varidx;
                transfer[2] = (double) num_obs[ivar];
                transfer[3] = max[ivar];
                transfer[4] = min[ivar];
                if (num_obs[ivar] == 0) {
                    transfer[1] = 0.0;
                } else {
                    transfer[1] = sum[ivar] / transfer[2];
                }
                return transfer[1];
            }
            // Initialize accumulators
            for (ivar = 1; ivar < svar_size_; ivar++) {
                sum[ivar] =  0.0;
                max[ivar] = -1.E30;
                min[ivar] =  1.E30;
                num_obs[ivar] = 0;
            }
            return 0.0;
        }

        double timest(double value, int varidx) {

        /* Initialize, update, or report statistics on continuous-time processes:
         * integral/average, max (default -1E30), min (default 1E30)
         * for timest variable "variable", where "variable":
         * = 0 initializes counters
         * > 0 updates area, min, and max accumulators with new level of variable
         * < 0 reports stats on variable “variable” and returns them in transfer:
         *      [1] = time-average of variable updated to the time of this call
         *      [2] = maximum value variable has attained
         *      [3] = minimum value variable has attained
         * Note that variables tim_var_ + 1 through tvar_size_ are used for automatic
         * record keeping on the length of lists 1 through maxlist_. */

            int ivar;
            static double area[tvar_size_], max[tvar_size_], min[tvar_size_];
            static double preval[tvar_size_], tlvc[tvar_size_], treset;
            if (varidx > 0) {
                area[varidx] += (simtime_ - tlvc[varidx]) * preval[varidx];
                if (value > max[varidx]) {
                    max[varidx] = value;
                }
                if (value < min[varidx]) {
                    min[varidx] = value;
                }
                preval[varidx] = value;
                tlvc[varidx]   = simtime_;
                return 0.0;
            }
            if (varidx < 0) {
                ivar = -varidx;
                area[ivar] += (simtime_ - tlvc[ivar]) * preval[ivar];
                tlvc[ivar]  = simtime_;
                transfer[1] = area[ivar] / (simtime_ - treset);
                transfer[2] = max[ivar];
                transfer[3] = min[ivar];
                return transfer[1];
            }
            // Initialize accumulators
            for (ivar = 0; ivar < tvar_size_; ivar++) {
                area[ivar] =  0.0;
                max[ivar]  = -1.E30;
                min[ivar]  =  1.E30;
                preval[ivar] = 0.0;
                tlvc[ivar]   = simtime_;
            }
            treset = simtime_;
            return 0.0;
        }

        double filest(int i) {

        /* Report statistics on the length of list "list" in transfer:
         *      [1] = time-average of list length updated to the time of this call
         *      [2] = maximum length list has attained
         *      [3] = minimum length list has attained
         * This uses timest variable TIM_VAR + list. */

            return timest(0.0, -(tim_var_+i));
        }

        // Wrappers for R
        void insert(int i, bool ascending=true, unsigned short option=0) {
            // this->transfer = as< std::vector<double> >(transfer);
            if (option == 0) {
                list_insert(i, ascending);
            } else if (option == 1) {
                list_insert_first(i);
            } else if (option == 2) {
                list_insert_last(i);
            }
        }

        void remove(int i, bool first) {
            list_remove(i,first);
            // return wrap(this->transfer);
        }

        // R printing utility
        void print_list(int i) {
            master *curr = head[i];
            while (curr != NULL) {
                for (int j = 0; j < maxatr_; j++) {
                    Rcout << curr->data[j] << " ";
                }
                curr = curr->next;
            }
            Rcout << std::endl;
        }

        // TEST ONLY
        void copyrvec(int i, NumericVector &transfer) {
            if (transfer.length()-1 > maxatr_) {
                transfer = transfer[Rcpp::Range(0,maxatr_)];
            }
            // long n   = std::min(transfer.length()-1,(long)maxatr_);
            // transfer = transfer[Rcpp::Range(0,n)];
            std::vector<double> x = as< std::vector<double> >(transfer);
            copyvec(i,x);
        }
};


RCPP_MODULE(simlib) {
  class_<DES>("DES")
  .constructor()
  .constructor<int>()
  .constructor<int,int>()
  .method("maxlist",             &DES::get_maxlist)
  .method("insert",              &DES::insert)
  .method("remove",              &DES::remove)
  .method("set_simtime",         &DES::set_simtime)
  .method("get_simtime",         &DES::get_simtime)
  .method("get_listsize",        &DES::get_listsize)
  .method("get_next_event_type", &DES::get_next_event_type)
  .method("set_next_event_type", &DES::set_next_event_type)
  .method("set_transfer",        &DES::set_transfer)
  .method("get_transfer",        &DES::get_transfer)
  .method("timing",              &DES::timing)
  .method("event_schedule",      &DES::event_schedule)
  .method("event_cancel",        &DES::event_cancel)
  .method("sampst",              &DES::sampst)
  .method("timest",              &DES::timest)
  .method("filest",              &DES::filest)
  // .method("freemem",             &DES::freemem)
  .method("print_list",          &DES::print_list)
  ;
}

