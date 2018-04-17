%%=======================================================================
%% Funcion para obtener el i-esimo elemento de una matriz de transicion
%% regular
%%=======================================================================
function value = pss (p,i)
  n=size(p); %Dimension
  identity=eye(2); %Identidad
  paux=p; %Auxiliar, para poner columnas en cero
  paux(:,i)=0;
  numerator=det(paux-identity);
  denominator=0;
  for j=1:n
    paux=p;
    paux(:,j)=0;
    denominator=denominator + det(paux-identity);
  endfor
  value=numerator/denominator;
endfunction