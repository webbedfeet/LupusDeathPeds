
# Workflows
data:  study_info fig_metadata update_study_info
figdata: fig_metadata KM_digitized
survdat: KM2IPD summ2IPD fup


## study_info		: Create data from study characteristics
study_info: 	 study_info.R
	R CMD BATCH study_info.R

update_study_info: study_info.R csvfiles.R
	R CMD BATCH DataMunging.R

## fig_metadata 		: Generate metadata for digitized curves
fig_metadata: csvfiles.R
	R CMD BATCH $<

## KM_digitized  		: Clean and store digitized curves
KM_digitized: Cleaning_KM.R data/rda/fig_metadata.rda
	R CMD BATCH $<

## KM2IPD  		: Generate IPD from clean KM curves
KM2IPD: KM2IPD.R data/rda/KM_digitized.rda
	R CMD BATCH KM2IPD.R

## summ2IPD : Generate IPD from summary data (graphical and spreadsheet)
summ2IPD: SummarySurvival.R
	R CMD BATCH SummarySurvival.R

## fup : Extract data for studies with only follow-up information
fup: FollowupData.R
	R CMD BATCH FollowupData.R

## membership : Generate which study belongs in which moving avg window
membership: DataMunging.R
	R CMD BATCH DataMunging.R

## Creating MCMC data for adult study
adult_data: MovingAverage.R data/rda/KM2IPD.rda data/rda/summaries2IPD.rda
	R CMD BATCH MovingAverage.R
	cd data/mcmc/adult; python template.py; cd ../../..
	cd data/mcmc/adult_10; python template.py; cd ../../..
	cd data/mcmc/inception; python template.py; cd ../../..





.PHONY : clean
clean:
	-rm  -f *.Rout *~

help: Makefile
		@sed -n 's/^##//p' $<
