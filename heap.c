#include <stdio.h>

/* Heap & Heap sort implementation */

#define PQ_MAX 1000

typedef int item_type;

typedef struct {
	item_type q[PQ_MAX+1];
	int n;
} priority_queue;


int pq_parent(int n)
{
	if (n == 1) return (-1);
	else return ((int) n / 2); // implicitly take floor (n/2)
}

int pq_child(int n) 
{
	return (2 * n);
}


void pq_swap(priority_queue *q, int index_a, int index_b)
{
	item_type temp = q->q[index_a];
	q->q[index_a] = q->q[index_b];
	q->q[index_b] = temp; 
} 

void bubble_up(priority_queue *q, int position)
{
	int parent = pq_parent(position);
	if (parent == -1) return; /* at the root of tree -- no parent */

	if (q->q[parent] > q->q[position]) 
	{
		pq_swap(q, parent, position);
		bubble_up(q, parent);
	}
}
void pq_insert(priority_queue *q, item_type x)
{
	if (q->n >= PQ_MAX)
	{
		printf("Warning. insertion exceeds maximum queue size");
	}
	else 
	{
		q->n = (q->n) + 1;
		q->q[q->n] = x;
		bubble_up(q, q->n);
	}
}

void pq_init(priority_queue *q)
{
	q->n = 0;
}

void construct_heap(priority_queue *q, item_type s[], int n)
{
	pq_init(q);
	int i;
	for(i=0; i<n; i++)
	{
		pq_insert(q, s[i]);
	}
}

void bubble_down(priority_queue *q, int position)
{
	/*
		Root of min-heap should dominate both its children
	*/
	int i;
	int child = pq_child(position);
	int min_index = position;
	
	for(i=0; i<=1; i++)
	{
		if ((child + i) <= q->n) 
		{
			if (q->q[min_index] > q->q[child + i])
				min_index = child + i;
		}
	}
	if (min_index != position)
	{
		pq_swap(q, position, min_index);
		bubble_down(q, min_index);
	}
}

item_type pop_min(priority_queue *q) 
{
	item_type min;
	if (q->n == 0)
	{
		printf("Empty priority queue\n");
	}
	else 
	{
		min = q->q[1];
		q->q[1] = q->q[q->n];
		q->n = q->n - 1;
		bubble_down(q, 1);
	}

	return(min);
}
void print_heap(priority_queue *q)
{
	int i;
	for(i=1; i <= q->n; i++)
	{
		printf("%d", q->q[i]);
	}
	printf("\n");
}

void print_arr(item_type s[], int n)
{
	int i;
	for(i=0; i<n; i++)
	{
		printf("%d ", s[i]);
	}
}

void heapsort(item_type s[], int n)
{
	priority_queue q;
	construct_heap(&q, s, n);
	int i = 0;
	while (q.n > 0)
	{
		s[i] = pop_min(&q);
		i++;
	}	
}


int main()
{
	int test[] = {9,6,7,8,1,3,4};
	heapsort(test, 7);
	print_arr(test, 7);
	// construct_heap(&p, test, 7);
	// item_type min = p.q[1];
	// printf("%d", min);
	// print_heap(&p);
	printf("\n");
	return(1);
}