function sigmatilde=analytical_shrinkage(X)
% extract sample eigenvalues sorted in ascending order and eigenvectors
[n,p]=size(X); % important: sample size n must be >= 12
sample=(X'*X);%./n;
[u,lambda]=eig(sample,'vector');
[lambda,isort]=sort(lambda);
u=u(:,isort);
% compute analytical nonlinear shrinkage kernel formula
lambda=lambda(max(1,p-n+1):p);
L=repmat(lambda,[1 min(p,n)]);
h=n^(-1/3);
H=h*L';
x=(L-L')./H;
ftilde=(3/4/sqrt(5))*mean(max(1-x.^2./5,0)./H,2);
Hftemp=(-3/10/pi)*x+(3/4/sqrt(5)/pi)*(1-x.^2./5) ...
   .*log(abs((sqrt(5)-x)./(sqrt(5)+x)));
Hftemp(abs(x)==sqrt(5))=(-3/10/pi)*x(abs(x)==sqrt(5));
Hftilde=mean(Hftemp./H,2);
if p<=n
   dtilde=lambda./((pi*(p/n)*lambda.*ftilde).^2 ...
      +(1-(p/n)-pi*(p/n)*lambda.*Hftilde).^2);
else
   Hftilde0=(1/pi)*(3/10/h^2+3/4/sqrt(5)/h*(1-1/5/h^2) ...
*log((1+sqrt(5)*h)/(1-sqrt(5)*h)))*mean(1./lambda); 
dtilde0=1/(pi*(p-n)/n*Hftilde0); 
dtilde1=lambda./(pi^2*lambda.^2.*(ftilde.^2+Hftilde.^2)); % Eq. (C.4) dtilde=[dtilde0*ones(p-n,1);dtilde1];
% Equation (4.3)
end
sigmatilde=u*diag(dtilde)*u';

end