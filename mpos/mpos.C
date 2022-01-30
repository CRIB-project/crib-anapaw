#include<TFile.h>
#include<fstream>

void Usage(){
   cout << "mpos calibration" << endl;
   cout << "Usage:" << endl;
   cout << "mpos(const Int_t np=0, TString run)" << endl;
   cout << "\tnp: number of peaks.  Default: 0 (this message).\n";
   cout << "\trun: Name of run file (DO NOT PUT '.root'). Default: nothing.  Must be in double quotes!\n";
}

void mpos(const Int_t np=0, TString run=" ")
{
  if (np==0){
    Usage();
    return;
  }
  gStyle->SetOptLogy(1);

  cout << endl;
  cout << "your input :" << endl;
  cout << "number of peaks : " << np << endl;
  cout << "run (filename) : " << run << ".root" <<endl;
  cout << endl;

  TString run_name = "/Data/si26a/rootfiles/" + run + ".root";
  FileStat_t info;
  if (gSystem->GetPathInfo(run_name.Data(), info)!=0) {
    cout << "File '" << run_name.Data() << "' does not exist." << endl;
    return;
  }

  TFile *fin = new TFile(run_name.Data(), "read");
  TTree* tree=(TTree*)fin->Get("tree");
  //must be changed
  TString output = run + "-mpos.log";
  cout << "CREATED: " << output << endl;
  ofstream fout(output.Data());
  fout << setprecision(8); // good for column output data

  TH1D *h[6];
  //must be changed
  Int_t ch_start[6] = {300, 650, 850, 1250, 1400, 1600};
  Int_t ch_stop[6] = {700, 1050, 1200, 1650, 1750, 1950};
  Int_t ch_interval[6] = {400, 400, 350, 400, 350, 350};
  for(Int_t i=0; i<6; i++){
    TString tmp;
    tmp.Form("h%d",i);
    h[i] = new TH1D(tmp.Data(), ";ch;entries", ch_interval[i], ch_start[i], ch_stop[i]);
    h[i]->GetXaxis()->SetRange(ch_start[i], ch_stop[i]);
    h[i]->GetXaxis()->SetRangeUser(ch_start[i], ch_stop[i]);
    h[i]->SetLineColor(9);
  }

  Int_t nfound;

  TString name = "madc2";
  Int_t num[32];
  tree->SetBranchAddress(name.Data(), num);
  Int_t nEntry = tree->GetEntries();
  for(Int_t iEntry=0; iEntry<nEntry; iEntry++){
    if(iEntry % (Int_t)(nEntry/10) == 0){
      cout << "roading... " << iEntry << " / " << nEntry << endl;
    }
    tree->GetEntry(iEntry);
    for(Int_t i=0; i<6; i++){
      if(i==2){
        h[i]->Fill(num[18]);
      }else{
        h[i]->Fill(num[4*i+2]);
      }
    }
  }

  Double_t high_ch[6] = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
  Double_t low_ch[6] = {10000.0, 10000.0, 10000.0, 10000.0, 10000.0, 10000.0};

  for(Int_t i=0; i<6; i++){
    cout << endl;
    Double_t xpeaks[np];
    TSpectrum *s = new TSpectrum(np);
    nfound = s->Search(h[i], 2, "", 0.01);
    cout << "Found " << nfound << " candidate peaks to fit" << endl;
    Double_t *xpeaksfound = s->GetPositionX();
    for(Int_t j=0; j<nfound; j++){
      if(xpeaksfound[j] > high_ch[i]){
        high_ch[i] = xpeaksfound[j];
      }
      if(xpeaksfound[j] < low_ch[i]){
        low_ch[i] = xpeaksfound[j];
      }
    }
    cout << "low_peak ch : " << low_ch[i] << "\thigh_peak ch : " << high_ch[i] << endl;

    if(nfound <= 0){
      cout << "No peaks found!" << endl;
    }
    if (nfound < np){
      cout << "COULD NOT FIND " << np << " peaks as user input requests!" << endl;
      if(i<5){
        fout << "WARNING:: COULD NOT FIND 16 peaks" << endl;
      }
      if(i==5 && nfound < np - 1){
        fout << "WARNING:: COULD NOT FIND 15 peaks" << endl;
      }
    }
  }

  fout << "please copy below parameters (it can rewrite the mssd.prm)" << endl;
  fout << "=================================================================" << endl;


  fout << "c lowest peaks" << endl;
  fout << "c " << run << endl;
  for(Int_t i=0; i<6; i++){
    if(i==5){
      fout << low_ch[i] << ", " << low_ch[i];
    }else{
      fout << low_ch[i] << ", " << low_ch[i] << ", ";
    }
  }
  fout << endl;
  fout << endl;
  fout << "c highest peaks" << endl;
  fout << "c " << run << endl;
  for(Int_t i=0; i<6; i++){
    if(i==5){
      fout << high_ch[i] << ", " << high_ch[i];
    }else{
      fout << high_ch[i] << ", " << high_ch[i] << ", ";
    }
  }

  TCanvas *c1 = new TCanvas("c1", "c1");
  c1->Divide(2, 3);
  for(Int_t i=0; i<6; i++){
    c1->cd(i+1);
    h[i]->Draw();
  }
  //c1->Print("./check.jpg");

  //fin->Close();
  fout.close();

}
