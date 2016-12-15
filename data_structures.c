#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef int item_type;
/* 

	Linked Lists

*/
typedef struct list {
	item_type item; 	/* data item */
	struct list *next;	/* point to successor */
} list;

list *search_list(list *l, item_type x) 
{
	if (l == NULL) return(NULL);

	if (l->item == x) 
		return (l);
	else
		return (search_list(l->next, x));
}

void insert_list(list **l, item_type x) 
{
	list *p;

	p = malloc(sizeof(list));
	p->item = x;
	p->next = *l;
	*l = p;
}


/*

	Stack

*/
typedef struct stack
{
	item_type item;
	struct stack *next;
} stack;

void push(stack *s, item_type x) 
{
	stack *p;
	p = malloc(sizeof(stack));
	p->item = x;
	s->next = p;
}
item_type pop(stack *s, item_type x)
{
	item_type t;
	t = s->item;
	free(s->next);
	return t;
}


int main()
{
	return 0;
}