#include <iostream>
#include <fstream>
#include <string>
#include <vector>
using namespace std;
class Nukleotyd
{
  public:
  int numer;
  char nukleotyd;
  int wiarygodnosc;
};
vector<Nukleotyd> sekwencja;
vector<vector<Nukleotyd>> instancja;
int Wczytaj_fasta()
{
  fstream plik;
  plik.open("sekwencje.fasta",ios::in);
  if (plik.good()==true)
  {
    string linia;
    bool zmienna=false;//pozwala uniknąć sytuacji gdy w pierwszej iteracji do tablicy o indeksie 0 przypisywana jest pusta lista
    int nr=1;
    cout<<"Plik fasta:"<<endl;
    while(getline(plik,linia))
    {
      auto it=linia.begin();
      if(*it!='>')
      {
        for(auto i=linia.cbegin(); i!=linia.cend(); ++i) 
        {
          Nukleotyd obiekt;
          obiekt.numer=nr;
          obiekt.nukleotyd=*i;
          cout<<*i;
          sekwencja.push_back(obiekt);
          nr++;
        }
        zmienna=true;
      }
      if(*it=='>'|| plik.eof()) 
        if(zmienna==true)
        {
        instancja.push_back(sekwencja);
        sekwencja.clear();
        nr=1;
        cout<<endl;
        }
    }
    plik.close();
    return 1;  
  }
  else
  { 
    cout<<endl<<"Brak dostepu do pliku fasta"<<endl; 
    return 0;
  }
}
int Wczytaj_qual()
{
  fstream plik;
  plik.open("wiarygodnosci.qual",ios::in);
  if (plik.good()==true)
  {
    string linia;
    bool zmienna=false;//pozwala uniknąć sytuacji gdy w pierwszej iteracji do tablicy o indeksie 0 przypisywana jest pusta lista
    int indeks=0;
    cout<<endl<<"Plik qual:"<<endl;
    auto iterator=instancja[indeks].begin();
    while(getline(plik,linia))
    {
      auto j=iterator;
      auto it=linia.begin();
      if(*it!='>')
      {
        linia=linia + ' ';//dodaje na koniec linii spacje, aby ostatnia wiarygodnosc tez byla wczytywana
        string liczba;
        for(auto i=linia.cbegin(); i!=linia.cend(); ++i) 
        {
          if(*i!=' ')
            liczba=liczba + *i;
          else
          { 
            int wartosc=atoi(liczba.c_str());
            j->wiarygodnosc=wartosc;
            cout<<wartosc<<" ";
            liczba.clear();
            j++;
            iterator=j;
          }
        }
        zmienna=true;
      }
      if(*it=='>'|| plik.eof()) 
        if(zmienna==true)
        {
          indeks++;
          iterator=instancja[indeks].begin();
          cout<<endl;
        }
    }
    plik.close();
    return 1;  
  }
  else
  { 
    cout<<endl<<"Brak dostepu do pliku qual"<<endl; 
    return 0;
  }
}
void Wyswietl()
{
  for(int i=0; i<5;i++)
  {
    cout<<endl<<"Numer sekwencji: "<<i+1<<endl;
    for (auto j= instancja[i].begin(); j!=instancja[i].end(); j++) 
      cout<<" "<<j->numer<<" "<<j->nukleotyd<<" "<<j->wiarygodnosc<<endl;
      //cout<<j->nukleotyd;
  }
  cout<<endl;
}
void Usuwanie(int n)
{
  for(int i=0; i<5;i++)
    for (auto j=instancja[i].begin(); j!=instancja[i].end(); j++) 
      if(j->wiarygodnosc<n)
      {  
        //cout<<j->wiarygodnosc<<" ";
        instancja[i].erase(j);
        j--;
      }
}
class Podciag
{
  public:
  int ID;
  int sekwencja;
  int poczatek;
  string podciag;
  vector<Podciag> sasiedztwo;
};
vector<Podciag> podciagi;
void Podciagi(int dlugosc)
{
  string ciag;
  int nr=0;
  for(int i=0; i<5;i++)
    for(auto j=instancja[i].begin(); j!=instancja[i].end()-(dlugosc-1); j++)
    {
      for(auto it=j; it<j+dlugosc; it++)
        ciag=ciag + (it->nukleotyd);
      nr++;
      Podciag obiekt;
      obiekt.ID=nr;
      obiekt.sekwencja=i+1;
      obiekt.poczatek=j->numer;
      obiekt.podciag=ciag;
      ciag.clear();
      podciagi.push_back(obiekt);
    }
}
void Wyswietl_podciagi()
{
  for(int i=0; i<podciagi.size();i++)
    cout<<podciagi[i].sekwencja<<" "<<podciagi[i].poczatek<<" "<<podciagi[i].podciag<<" "<<podciagi[i].ID<<endl;
}
int macierz[600][600];
void Krawedzie()
{
  for(int i=0; i<podciagi.size();i++)
    for(int j=i+1; j<podciagi.size();j++)
      if(podciagi[i].podciag==podciagi[j].podciag && podciagi[i].sekwencja!=podciagi[j].sekwencja)
      { 
        macierz[podciagi[i].ID][podciagi[j].ID]=1;
        macierz[podciagi[j].ID][podciagi[i].ID]=1;
        Podciag obiekt1;
        obiekt1.sekwencja=podciagi[j].sekwencja;
        obiekt1.poczatek=podciagi[j].poczatek;
        obiekt1.podciag=podciagi[j].podciag;
        obiekt1.ID=podciagi[j].ID;
        podciagi[i].sasiedztwo.push_back(obiekt1);
        Podciag obiekt2;
        obiekt2.sekwencja=podciagi[i].sekwencja;
        obiekt2.poczatek=podciagi[i].poczatek;
        obiekt2.podciag=podciagi[i].podciag;
        obiekt2.ID=podciagi[i].ID;
        podciagi[j].sasiedztwo.push_back(obiekt2);
      }
}
void Wyswietl_sasiedztwo()
{
  cout<<endl<<"Lista sasiedztwa dla wierzcholkow o takich samych podciagach:";
  for(int i=0; i<podciagi.size();i++)
  {
    cout<<endl<<podciagi[i].sekwencja<<" "<<podciagi[i].poczatek<<" "<<podciagi[i].podciag<<":";
    for(auto it=podciagi[i].sasiedztwo.begin(); it!=podciagi[i].sasiedztwo.end(); it++) 
        cout<<" "<<it->sekwencja<<" "<<it->poczatek<<" "<<it->podciag<<",";
  }
}
int klika[5];
int Klika()
{
  int j;
  for(int i=0; i<podciagi.size();i++)
  {
    int stopien_w=podciagi[i].sasiedztwo.size();
    if(stopien_w>=4)
    {
      j=0;
      klika[j]=podciagi[i].ID;
      j++;
      for(auto it=podciagi[i].sasiedztwo.begin(); it!=podciagi[i].sasiedztwo.end(); it++) 
      {
        if(macierz[it->ID][podciagi[i].ID])
        {
          //sprawdzenie czy sasiedzi pochodza z roznych sekwencji, jesli sa z tej samej to przechodzimy do kolejnego, bez dodawania tego na ktorym jestesmy
          if(j>=2 && macierz[it->ID][klika[j-1]]==1 && macierz[klika[j-1]][it->ID]==1)
          {
            klika[j]=it->ID;
            j++;
          }
          if(j<2)
          {
            klika[j]=it->ID;
            j++;
          }
        }
      }
      int licz=0;
      for(int iterator=0; iterator<5; iterator++)
        if(klika[iterator]==0)
          licz++;
      if(licz==0)
      return 1;
    }
  }
  return 0;
}
void Wyswietl_klike() 
{ 
  int a=1;
  for (int i = 0; i<5; i++)
    for(int j=0; j<podciagi.size();j++) 
      if(klika[i]==podciagi[j].ID)
      {
        if(a==1)
        {
          cout<<endl<<endl<<"Znaleziona klika zawiera podciag: "<<podciagi[j].podciag<<endl;
          cout<<"Nr sekw.:    "<<"Nr poz. w sekw.: "<<endl;
          a=0;
        }
        cout<<"     "<<podciagi[j].sekwencja<<"              "<<podciagi[j].poczatek<<endl;
      } 
}
int main() 
{
  if(Wczytaj_fasta()==0)
    return 0;
  if(Wczytaj_qual()==0)
    return 0;
  //Wyswietl();
  int prog;
  cout<<"Podaj prog: ";
  cin>>prog;
  Usuwanie(prog);
  Wyswietl();
  int dlugosc;
  cout<<endl<<"Wprowadź dlugosc podciagu: ";
  cin>>dlugosc;
  Podciagi(dlugosc);
  //Wyswietl_podciagi();//wyswietla sie tutaj tez numer ID podciagu
  Krawedzie();
  Wyswietl_sasiedztwo();
  if(Klika()==1)
    Wyswietl_klike();
  else
    cout<<endl<<endl<<"Nie znaleziono kliki.";
  return 0;
}