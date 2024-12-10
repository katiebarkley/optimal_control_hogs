% Parameters 
aspace = linspace(1,2,6); 
sspace = linspace(1,50,15); 

p = 0.1;                % dollars per bushel
Y = 12500;              % yield
d = 0.25;               % damage proportion
K = 50;                 % CC for the farm
r = 0.584;              % growth rate
c1 = 20;                % variable cost
c0 = 100;               % fixed cost
delta = 0.1;            % discount factor
s1 = 600;               % fixed farming cost
mu = 0.05;               % variable farming cost based on hog damage
a  = 1;                  % avoidance
beta = 2;               % damage intensity

%%
% Define the ftrfun function
ftrfun = @(s) (-p*d*beta*((s-r*s*(1-s/K)).^(beta-1)) - s1*mu/K + a*c1*r*s*(1-s/K)./(s.^2)) ...
               ./ (p*d*beta*((s-r*s*(1-s/K)).^(beta-1)) - a*c1./s) + r - 2*r*s/K - delta;

% Vector of population values
s = linspace(0.001, K, 100);

% Evaluate the function over the population values
ssval = arrayfun(ftrfun, s);

% Finding the steady state
sstar = fzero(ftrfun, [0.1, 10]);

% Solving the appropriate x to match the population
xstar = r * sstar * (1 - sstar / K);

% Define the hfind function
hfind = @(h) (-p*d*beta*((sstar-h).^(beta-1)) - s1*mu/K + a*c1*h/(sstar^2)) ...
              ./ (p*d*beta*((sstar-h).^(beta-1)) - a*c1/sstar) + r - 2*r*sstar/K - delta;

% Finding the optimal harvest value
optimal_harvest = fzero(hfind, [0.1, 25]);


%%
% Plotting the fundamental theorem of renewable resources
figure
plot(s, ssval,'k' ,'LineWidth', 2)
hold on;
xlim([0, 10]);
ylim([-1, 1]);
yline(0, 'k--');
xlabel("Population");
title("Fundamental Theorem of Renewable Resources")
[~, idx] = min(abs(ssval));
plot(s(idx), 0, 'ko', 'MarkerFaceColor', 'k', 'MarkerSize', 8)
hold on;
xline(s(idx),'r--');

%save plot
%fname = strcat('NHplot_a',num2str(a),'_init',num2str(sinit),'.png') ;
%saveas(gcf,fname)

%%
% Solution path
tmax = 50;          % Time
spath = zeros(1, tmax); % Population vector
xpath = zeros(1, tmax); % Harvest vector
spath(1) = sstar;    % Initial population value: want to range this 
xpath(1) = optimal_harvest;
for i = 1:tmax-1
     spath(i+1) = spath(i) + r*spath(i)*(1-spath(i)/K) - xpath(i); % Growth equation
    if spath(i+1) < 0
        spath(i+1) = 0; % Non-negativity constraint
    end
    st = spath(i+1);
    if spath(i+1) ~= 0 
    hfind = @(h) (-p*d*beta*((st-h).^(beta-1)) - s1*mu/K + a*c1*h/(st^2)) ...
                  ./ (p*d*beta*((st-h).^(beta-1)) - a*c1/st) + r - 2*r*st/K - delta;
    xpath(i+1) = fzero(hfind, [0.01, 25]);
    end
end

%%

% Plot population vs harvest
figure;
plot(spath, xpath, 'LineWidth', 2)
xlim([0, 50]);
ylim([0, 25]);
xlabel('Population');
ylabel('Harvest');
hold on;
xline(sstar, 'k--');
yline(xstar, 'k--');

%save plot
%fname = strcat('NHplot_a',num2str(a),'_init',num2str(sinit),'.png') ;
%saveas(gcf,fname)


% Plot spath and xpath over time
figure;
plot(1:tmax, spath, 'LineWidth', 2)
hold on;
plot(1:tmax, xpath, 'r', 'LineWidth', 2)
xlim([0, 50]);
ylim([0, 20]);
xlabel('Time');
ylabel('Population and Harvest');
title('Time Path of Harvest and Population');
yline(sstar, 'k--');
yline(xstar, 'r--');
legend('Population', 'Harvest', 'Population SS', 'Harvest SS', 'Location', 'best');

%save plot
%fname = strcat('NHplot_a',num2str(a),'_init',num2str(sinit),'.png') ;
%saveas(gcf,fname)

