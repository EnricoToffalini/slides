#### FingerTapping.csv
- ID: Unique identification codes for participants, anonymized to ensure privacy.
- Order: The sequence of emotional video presentations; possible values are: "PNN" for Positive-Neutral-Negative or "NNP" for Negative-Neutral-Positive.
- Emotion: The type of emotional video associated with the response; possible values: are "Positive", "Neutral", or "Negative".
- PrePost: Indicates whether the tapping was performed before or after emotional video immersion; possible values: "Pre" or "Post".
- Trial: The trial number (note: it is recommended to exclude the first trial for each participant from analysis). 
- ITI: "Inter-Tap-Interval": represents the time (in milliseconds) between one tap and the previous tap. 

#### TimePerception.csv
- ID: Unique identification codes for participants, anonymized to ensure privacy.
- Order: The sequence of emotional video presentations; possible values are: "PNN" for Positive-Neutral-Negative or "NNP" for Negative-Neutral-Positive.
- Emotion: The type of emotional video associated with the response; possible values: are "Positive", "Neutral", or "Negative".
- Retro_Estimate: "Retrospective estimate" of the perceived duration of the video, as reported by participants (measured in minutes).
- Prosp_Estimate: "Prospective estimate" of the time when participants felt one minute had passed since seeing the red dot (measured in milliseconds).

#### PhysiologicalDataProcessesd.csv
- ID: Unique identification codes for participants, anonymized to ensure privacy.
- Order: The sequence of emotional video presentations; possible values are: "PNN" for Positive-Neutral-Negative or "NNP" for Negative-Neutral-Positive.
- Recording: The observation condition, which may be "Resting" (baseline) or one of three types of emotional video associated with the response that are "Positive", "Neutral", or "Negative".
- Minute: The minute epoch in which the observation was taken and aggregated; takes integer values in [1, 5]. 
- HR: Heart Rate (beats per minute) observed in the given minute epoch.
- SCR: Average Skin Conductance Response (SCR) observed in the given minute epoch, measured in nanosiemens (nS).


