using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.IO;
using System.Diagnostics;

namespace SortingAlgorithms
{
    class Program
    {
        static void Main(string[] args)
        {
            PerformSorts("sorted.txt");
            PerformSorts("nearly-sorted.txt");
            PerformSorts("duplicate.txt");
            PerformSorts("shuffled.txt");
            PerformSorts("nearly-unsorted.txt");
            PerformSorts("unsorted.txt");
            Console.ReadKey(true);
        }

        static List<int> ReadFile(string fileName)
        {
            var numberList = new List<int>();
            string line;

            TextReader reader = File.OpenText("data/" + fileName);
            reader.ReadLine();  // skip first line
            int total = int.Parse(reader.ReadLine());   // second line gives total numbers

            // read the file
            while ((line = reader.ReadLine()) != null)
            {
                numberList.Add(int.Parse(line));
            }

            return numberList;
        }

        static void PerformSorts(string fileName)
        {
            var Sort = new Sorter<int>(ReadFile(fileName));
            long sum;

            Console.WriteLine("--------------- " + fileName + " ---------------");
            List<int> insertionSort = Sort.InsertionSort();
            Console.WriteLine("Insertion Sort");
            Console.WriteLine("Comparisons: " + Sort.Comparisons);
            Console.WriteLine("Assignments: " + Sort.Assignments);
            sum = Sort.Comparisons + Sort.Assignments;
            Console.WriteLine("Total Operations: " + sum);
            Console.WriteLine();
            List<int> mergeSort = Sort.MergeSort();
            Console.WriteLine("Merge Sort");
            Console.WriteLine("Comparisons: " + Sort.Comparisons);
            Console.WriteLine("Assignments: " + Sort.Assignments);
            sum = Sort.Comparisons + Sort.Assignments;
            Console.WriteLine("Total Operations: " + sum);
            Console.WriteLine();
            List<int> countingSort = Sort.CountingSort();
            Console.WriteLine("Counting Sort");
            Console.WriteLine("Comparisons: " + Sort.Comparisons);
            Console.WriteLine("Assignments: " + Sort.Assignments);
            sum = Sort.Comparisons + Sort.Assignments;
            Console.WriteLine("Total Operations: " + sum);
            Console.WriteLine();
            List<int> shellSort = Sort.ShellSort();
            Console.WriteLine("Shell Sort");
            Console.WriteLine("Comparisons: " + Sort.Comparisons);
            Console.WriteLine("Assignments: " + Sort.Assignments);
            sum = Sort.Comparisons + Sort.Assignments;
            Console.WriteLine("Total Operations: " + sum);
            Console.WriteLine();
        }
    }
}
