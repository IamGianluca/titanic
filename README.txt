This is an analysis based on the data made available by Kaggle
(www.kaggle.com) on the Titanic distaster competition. This work aims to
predict what sorts of people were more likely to survive.

On the folder text you can find the R Markdown file to generate a pdf 
version of the final report, which summarises the key findings of our
reseach. 

On the folder R code you can find two sub-folders. Raw scripts 
contains all tests we conducted and it is not intended to be a place
where find perfectly formatted and optimised R code. Whereas final
scripts include the final version of R code we wrote to conduct
our analysis. We advice you to give it a read to have a better 
overview of how the analysis has been conducted.

Data contains both the raw dataset we downloaded from Kaggle.com and 
a tiny dataset with variables transformed in the format we used when
conducting our analysis.

#####

VARIABLES DESCRIPTION:

survival        Survival
                (0 = No; 1 = Yes)
pclass          Passenger Class
                (1 = 1st; 2 = 2nd; 3 = 3rd)
name            Name
sex             Sex
age             Age
sibsp           Number of Siblings/Spouses Aboard
parch           Number of Parents/Children Aboard
ticket          Ticket Number
fare            Passenger Fare
cabin           Cabin
embarked        Port of Embarkation
                (C = Cherbourg; Q = Queenstown; S = Southampton)

#####

SPECIAL NOTES:

Pclass is a proxy for socio-economic status (SES)
 1st ~ Upper; 2nd ~ Middle; 3rd ~ Lower

Age is in Years; Fractional if Age less than One (1)
 If the Age is Estimated, it is in the form xx.5

With respect to the family relation variables (i.e. sibsp and parch)
some relations were ignored.  The following are the definitions used
for sibsp and parch.

Sibling:  Brother, Sister, Stepbrother, or Stepsister of Passenger Aboard Titanic
Spouse:   Husband or Wife of Passenger Aboard Titanic (Mistresses and Fiances Ignored)
Parent:   Mother or Father of Passenger Aboard Titanic
Child:    Son, Daughter, Stepson, or Stepdaughter of Passenger Aboard Titanic

Other family relatives excluded from this study include cousins,
nephews/nieces, aunts/uncles, and in-laws.  Some children travelled
only with a nanny, therefore parch=0 for them.  As well, some
travelled with very close friends or neighbors in a village, however,
the definitions do not support such relations.
