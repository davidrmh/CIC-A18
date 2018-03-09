

def createC1(dataset):
    '''
    Creates candidate itemset of size one
    dataset is a list of list where each list is
    a transaction and the element inside of that list
    is the article id.
    '''
    c1=[]
    for transaction in dataset:
        for item in transaction:
            if not [item] in c1:
                c1.append([item])
    c1.sort()
    return map(frozenset,c1)

def scanD(dataset,candidates,minsupport):
    '''
    Calculates support for every itemset in candidates
    dataset is created with map(set,dataset)
    where dataset is a list of list.

    Returns
    retlist: A list containing the itemsets that passed the minimum support
    supportdata: A dictionary with the support of every itemset
    '''
    sscnt={}
    for tid in dataset:
        for can in candidates:
            if can.issubset(tid): #If itemset is present in transaction
                if not sscnt.has_key(can): sscnt[can]=1
                else: sscnt[can] +=1
    numitems=float(len(dataset))
    retlist=[]
    supportdata={}
    for key in sscnt:
        support = sscnt[key]/numitems
        if support>=minsupport:
            retlist.insert(0,key) #inserts from the left
        supportdata[key]=support
    return retlist,supportdata

def aprioriGen(lk,k):
    retlist=[]
    lenlk=len(lk)
    for i in range(lenlk):
        for j in range(i+1,lenlk):
            l1=list(lk[i])[:k-2]
            l2=list(lk[j])[:k-2]
            l1.sort()
            l2.sort()
            if l1==l2:
                retlist.append(lk[i]|lk[j])
    return retlist

def apriori(dataset,minsupport=0.5):
    c1=createC1(dataset)
    d=map(set,dataset)
    l1,supportdata=scanD(d,c1,minsupport)
    l=[l1]
    k=2
    while(len(l[k-2])>0):
        ck=aprioriGen(l[k-2],k)
        lk,supk=scandD(d,ck,minsupport)
        supportdata.update(supk)
        l.append(lk)
        k+=1
    return l,supportdata

def generateRules(l,supportdata,minconf=0.7):
    bigrulelist=[]
    for i in range(1,len(l)):
        for freqset in l[i]:
            h1=[frozenset([item]) for item in freqset]
            if(i>1):
                rulesfromconseq(freqset,h1,supportdata,bigrulelist,minconf)
            else:
                calcconf(freqset,h1,supportdata,bigrulelist,minconf)
    return bigrulelist

def calcconf(freqset,h,supportdata,brl,minconf=0.7):
    prunedh=[]
    for conseq in h:
        conf=supportdata[freqset]/supportdata[freqset-conseq]
        if conf>= minconf:
            print freqset-conseq,'-->',conseq,'conf:',conf
            brl.append((freqset-conseq,conseq,conf))
            prunedh.append(conseq)
    return prunedh

def rulesfromconseq(freqset,h,supportdata,brl,minconf=0.7):
    m=len(h[0])
    if (len(freqset)>(m+1)):
        hmp1=aprioriGen(h,m+1)
        hmp1=calcconf(freqset,hmp1,supportdata,brl,minconf)
        if(len(hmp1)>1):
            rulesfromconseq(freqset,hmp1,supportdata,brl,minconf)
