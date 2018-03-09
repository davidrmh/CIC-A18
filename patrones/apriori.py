

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
