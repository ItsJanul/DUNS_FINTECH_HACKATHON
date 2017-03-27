# DUNS_FINTECH_HACKATHON
## Cornell Fintech Hackathon 2016- Team DUNS


This is the machine learning model and presentation that won runner up at [Cornell Tech's 2016 Fintech Hackathon](https://fintechhackathon2016.splashthat.com/)


## Data
We based our dataset on PKDD'99 Discovery Challenge data set.

Relations within the accounts:
- relation account (4500 objects in the file ACCOUNT.ASC) - each record describes static characteristics of an account,
- relation client (5369 objects in the file CLIENT.ASC) - each record describes characteristics of a client,
- relation disposition (5369 objects in the file DISP.ASC) - each record relates together a client with an account i.e. this relation describes the rights of clients to operate accounts,
- relation permanent order (6471 objects in the file ORDER.ASC) - each record describes characteristics of a payment order,
- relation transaction (1056320 objects in the file TRANS.ASC) - each record describes one transaction on an account,
- relation loan (682 objects in the file LOAN.ASC) - each record describes a loan granted for a given account,
- relation credit card (892 objects in the file CARD.ASC) - each record describes a credit card issued to an account,
- relation demographic data (77 objects in the file DISTRICT.ASC) - each record describes demographic characteristics of a district.

### We did not end up using all relations due to time constraints of the [Fintech Hackathon Challenge](https://fintechhackathon2016.splashthat.com/)


## Important links
- You can find the [datasets here](http://lisp.vse.cz/pkdd99/berka.htm) 
- You can find our [proto.io here](https://pr.to/HLERWZ/)
- You can find my [twitter here](https://twitter.com/itsjanul) for any suggestions! 
