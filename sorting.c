#include <stdio.h>
#include <string.h>
#define SIZE 10;

void swap(int* a, int* b)
{
	int t = *a;
	*a = *b;
	*b = t;
}

int * selection_sort(int arr[], int n)
{
	/* finds the min per iteration */
	int i, j, t;
	int min;

	for(i=0; i < n; i++)
	{
		min=i;
		for(j=i+1; j<n; j++)
		{
			if (arr[min] > arr[j]) min=j;
		}
		swap(&arr[min], &arr[i]);
	}

	return arr;
}

int * insertion_sort(int arr[], int n)
{
	/* bubbles num through */
	int i, j, t;
	for(i=0; i<n; i++)
	{
		j=i;
		while(j>0 && (arr[j] < arr[j-1]))
		{
			swap(&arr[j], &arr[j-1]);
			j--;
		}
	}
	return arr;
}


void show_array(int arr[], int size) 
{
	int i, j;
	for(i=0;i<size;i++)
	{
		printf("%d ", arr[i]);
	}
	printf("\n");
}

int findpattern(char *str, char *sub) 
{
	int n, m;
	int i, j, c;

	n = strlen(str);
	m = strlen(sub);

	for(i=0; i<(n-m); i++)
	{
		c=0;
		while(c<n && (str[i+c] == sub[c]))
			c++;
		if (c == m) return i;
	}
	return (-1);
}


int main () 
{
	int size = 10;
	int array[] = {9,7,8,5,6,3,4,2,1,0}; 
	show_array(selection_sort(array, size), size);
	show_array(insertion_sort(array, size), size);
	printf("%d\n", findpattern("aaaabbb", "ab"));
	return 0;
}