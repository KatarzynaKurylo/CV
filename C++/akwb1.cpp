#include <iostream>
#include <fstream>
#include <string>
#include <list>
#include <array>
using namespace std;
int n;
int m;
array<list<int>,20 > graf;
array<list<int>,20 > luki;
array<list<int>,20 > oryginalny;
int nastepniki[20][20];
int Wczytywanie_pliku()
{
  fstream plik;
  plik.open("Graf.txt",ios::in);
  if (plik.good()==true)
  {
    string liczba;
    int indeks=0;
    int wierzcholek;
    getline(plik,liczba);
    n=atoi(liczba.c_str());
    list<int> lista;
    while(plik>>liczba)
    {
      if(liczba!="*")
      {
        wierzcholek=atoi(liczba.c_str());
        lista.push_back(wierzcholek);
        graf[indeks]=lista;  
      }
      else
      {
        lista.clear();
        indeks++;
      }
    }
    plik.close();
    return 1;  
  }
  else
  { 
    cout<<endl<<"Brak dostepu do pliku"<<endl; 
    return 0;
  }
}
int Sprzezony()
{
  //sprawdzenie czy w grafie wystepuja petle wielokrotne
  for(int i=0; i<n; i++)
  {
    for(auto it=graf[i].begin(); it!=graf[i].end(); it++) 
    {
      for(auto iterator=graf[i].begin(); iterator!=graf[i].end(); iterator++)
      { 
        if(iterator!=it)
          if(*it==*iterator)
            return 0;
      }
    }
  }
  //sprawdzenie czy nastepniki pary wierzcholkow sa zbiorami rozlacznymi lub gdy jeden z nich sie powtarza, to czy sa zbiorami identycznymi
  for(int i=0; i<n-1; i++)
  {
    for(int j=i+1; j<n; j++)
    {
      for(auto it=graf[i].begin(); it!=graf[i].end(); it++) 
      {
        for(auto iterator=graf[j].begin(); iterator!=graf[j].end(); iterator++)
        {
          if(*it==*iterator && nastepniki[i][j]!=1)
          {
            int ilosc_porownan=0;
            size_t rozmiar1=graf[i].size();
            size_t rozmiar2=graf[j].size();
            if(rozmiar1==rozmiar2)
              for(auto a=graf[i].begin(); a!=graf[i].end(); a++) 
                for(auto b=graf[j].begin(); b!=graf[j].end(); b++)
                  if(*a==*b)
                    ilosc_porownan++;  
            if(rozmiar1==ilosc_porownan)
              nastepniki[i][j]=1;
            else
              return 0;
          }
        }
      }
    }
  }
  return 1;
}
int Liniowy()
{
  //jesli para wierzcholkow ma identyczne zbiory nastepnikow, a ich poprzedniki sa zbiorami rozlacznymi to graf jest liniowy
  for (int i=0; i<n; i++)
  {
    for (int j=0; j<n; j++)
    { 
      if(nastepniki[i][j]==1)
      {
        list<int> lista_poprzednikow;
        list<int> lista_poprzednikow1;
        for(int a=0; a<n; a++)
          for(auto it=graf[a].begin(); it!=graf[a].end(); it++)
            if(*it==i)
              lista_poprzednikow.push_back(a);
        for(int a=0; a<n; a++)
          for(auto it=graf[a].begin(); it!=graf[a].end(); it++)
            if(*it==j)
              lista_poprzednikow1.push_back(a);
        for(auto c=lista_poprzednikow.begin(); c!=lista_poprzednikow.end(); c++) 
          for(auto b=lista_poprzednikow1.begin(); b!=lista_poprzednikow1.end(); b++)
            if(*c==*b)
              return 0;             
      }
    }
  }
  return 1;
}
void Oryginalny()
{
  //tworze zbior rozlacznych lukow
  int x=0,y=1;
  for(int i=0; i<n; i++)
  {
    list<int> lista1;
    lista1.push_back(x); lista1.push_back(y);
    luki[i]=lista1;
    x=x+2; y=y+2;
  }
  //przeindeksowanie w zbiorze wszystkich wystapien starego wierzcholka na nowy
  for(int i=0; i<n; i++)
  {
    for(auto it=graf[i].begin(); it!=graf[i].end(); it++)
    {
      int poczatek=luki[*it].front();
      int koniec=luki[*it].back();
      int zamiana=luki[i].back();
      if(poczatek!=zamiana)
      {
        luki[*it].front()=zamiana;
        for(int j=0; j<n; j++)
          for(auto iterator=luki[j].begin(); iterator!=luki[j].end(); iterator++)
            if(*iterator==poczatek)
            {
              //cout<<endl<<*iterator<<" -> "<<zamiana;
              *iterator=zamiana;
            }
      }
    }
  }
  //indeksowanie wierzcholkow, zeby byly po kolei
  int b=-1;
  for(int j=0; j<n*2; j++)
  { 
    int a=40;
    for(int i=0; i<n; i++)
      for(auto it=luki[i].begin(); it!=luki[i].end(); it++)
        if(*it>b)
          if(*it<a)
            a=*it;
    if(a==40)
      break;
    m++;
    for(int i=0; i<n; i++)
      for(auto iter=luki[i].begin(); iter!=luki[i].end(); iter++)
        if(*iter==a)
        *iter=j;
    b=a;
  }
  /*for(int i=0; i<n; i++)
  {
    cout<<endl<<i<<": ";
    for(auto iter=luki[i].begin(); iter!=luki[i].end(); iter++)
      cout<<*iter<<" ";
  }*/
  for(int i=0; i<m; i++)
  { 
    list<int> lista2;
    for(int j=0; j<n; j++)
      if(luki[j].front()==i)
      {
        int w=luki[j].back();
        lista2.push_back(w);
      }
    oryginalny[i]=lista2;
  }
  cout<<endl;
  cout<<"Graf oryginalny: "<<endl;
  cout<<"Ilosc wierzcholkow: "<<m;
  for(int i=0; i<m; i++)
  { 
    cout<<endl<<i<<": ";
    for(auto iter=oryginalny[i].begin(); iter!=oryginalny[i].end(); iter++)
      cout<<*iter<<" ";
  }
}
void Zapisywanie_pliku()
{
  fstream plik;
  plik.open("Oryginalny.txt",ios::out);
  if (plik.good()==true)
  {
    cout<<endl<<endl<<"Wczytywanie do pliku"<<endl;
    plik<<m<<endl;
    for(int i=0; i<m; i++)
    {
      for(auto iter=oryginalny[i].begin(); iter!=oryginalny[i].end(); iter++)
        plik<<*iter<<" ";
      plik<<"*"<<endl;
    }
    plik.close();
  }
}
int main() 
{
  if(Wczytywanie_pliku()==0)
    return 0;
  cout<<"Ilosc wierzcholkow: "<<n;
  for(int i=0; i<n; i++)
  {
    cout<<endl<<i<<": ";
    for(auto it=graf[i].begin(); it!=graf[i].end(); it++)
      cout<<*it<<" ";
  }
  if(Sprzezony()==0)
  { 
    cout<<endl<<endl<<"Graf nie jest sprzezony!"<<endl; 
    return 0;
  }
  else
    cout<<endl<<endl<<"Graf jest sprzezony"<<endl;
  if(Liniowy()==0)
    cout<<endl<<"Graf nie jest liniowy"<<endl; 
  else
    cout<<endl<<"Graf jest liniowy"<<endl;
  Oryginalny();
  Zapisywanie_pliku();
  return 0;
}