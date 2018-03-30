using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Diagnostics.Contracts;

namespace SortingAlgorithms
{
    class Sorter<T> where T : struct, IComparable     // value type and comparable
    {
        private long comparisons;
        private long assignments;
        private List<T> list;
        private IList<T> originalList;

        public Sorter(IList<T> inputList)
        {
            this.originalList = inputList;
            this.list = CopyList(inputList);
        }

        public long Comparisons
        {
            get { return comparisons; }
            set { comparisons = value; }
        }

        public long Assignments
        {
            get { return assignments; }
            set { assignments = value; }
        }

        public bool IsSorted()
        {
            for (int i = 1;  i < list.Count; i++)
                if (list[i].CompareTo(list[i-1]) < 0)
                    return false;
            return true;
        }

        public List<T> InsertionSort()
        {
            Contract.Ensures(IsSorted());
            comparisons = 0;
            assignments = 0;
            list = CopyList(originalList);

            for (int j = 1; j < list.Count; j++)
            {
                var key = list[j];
                // Insert list[i] into the sorted sequence list[1...i-1]
                int i = j - 1;

                while (i >= 0 && less(key, list[i]))
                {
                    list[i + 1] = list[i];
                    assignments++;
                    i--;
                }
                list[i + 1] = key;
                assignments++;
            }
            return list;
        }

        public List<T> MergeSort()
        {
            Contract.Ensures(IsSorted());
            comparisons = 0;
            assignments = 0;
            list = CopyList(originalList);

            List<T> aux = new T[list.Count].ToList();
            MergeSort(aux, 0, list.Count - 1);
            return list;
        }

        private void MergeSort(List<T> aux, int lo, int hi)
        {
            
            if (hi <= lo) return;
            int mid = lo + (hi - lo) / 2;
            MergeSort(aux, lo, mid);
            MergeSort(aux, mid + 1, hi);
            merge(aux, lo, mid, hi);
        }

        public void merge(List<T> aux, int lo, int mid, int hi)
        {
            int i = lo, j = mid+1;

            for (int k = lo; k <= hi; k++)
            {
                aux[k] = list[k];
            }

            for (int k = lo; k <= hi; k++)
            {
                if (i > mid)
                    list[k] = aux[j++];
                else if (j > hi)
                    list[k] = aux[i++];
                else if (less(aux[j], aux[i]))
                    list[k] = aux[j++];
                else
                    list[k] = aux[i++];
                assignments++;
            }
        }

        public List<T> ShellSort()
        {
            Contract.Ensures(IsSorted());
            comparisons = 0;
            assignments = 0;
            list = CopyList(originalList);

            int N = list.Count;
            int h = 1;
            while (h < N / 3)
                h = 3 * h + 1;
            while (h >= 1)
            {
                for (int i = h; i < N; i++)
                    for (int j = i; j >= h && less(list[j], list[j - h]); j -= h)
                        exchange(j, j-h);
                h = h / 3;
            }
            return list;
        }

        public List<T> CountingSort()
        {
            Contract.Requires(typeof(T) == typeof(int));
            Contract.Ensures(IsSorted());
            comparisons = 0;
            assignments = 0;

            int[] countArray = new int[100];
            foreach (T num in originalList)
            {
                countArray[(int)(object) num]++;
            }

            list = new List<T>();

            for (int i = 0; i < 100; i++)
                for (int j = countArray[i]; j > 0; j--)
                {
                    list.Add((T)(object) i);
                    assignments++;
                }

            return list;
        }

        public void PrintSortedList()
        {
            foreach (T item in list)
            {
                Console.WriteLine(item);
            }
        }

        // Used so that we can return a sorted list without modifying original
        private List<T> CopyList(IList<T> list)
        {
            List<T> listCopy = new List<T>(list);
            return listCopy;
        }

        // return true if a is less than b
        private bool less(T a, T b)
        {
            comparisons++;
            return a.CompareTo(b) < 0;
        }

        // exchange a with b
        private void exchange(int a, int b)
        {
            T temp = list[a];
            list[a] = list[b];
            list[b] = temp;
            assignments += 2;   // 2 list assignments done
        }
    }
}