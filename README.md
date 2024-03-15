# RiskWeaver

A Domain Specific Language for the Specification of Risk-oriented Object Detection Requirements. Featuring:

* Relation-based Object Analysis
* Risk Factor Assessment
* Error Type Categorization
* Comprehensive Metrics
* Environment and Object Modeling
* Performance Evaluation Tools
* Data filtering
* Visualization

The formats of input/output data are based on [COCO](https://cocodataset.org/#format-data) and [COCO Results](https://cocodataset.org/#format-results).

## Getting Started

## Installation

1. Git clone the repository.
```bash
git clone https://github.com/jst-qaml/risk-weaver.git
```
2. Install the required packages using the following command:
```bash
#Install ghcup
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
#Install ghc and cabal
ghcup install ghc 9.6.3
ghcup set ghc 9.6.3
ghcup install cabal 3.10.2.0
ghcup set cabal 3.10.2.0
# Build the project
cabal update
cabal build all
cabal install
```
Then it installs risk-weaver-exe in the .cabal/bin directory.
When you update risk definision, you need to run the following command to update the risk-weaver-exe.

3. Generate a template code for the RiskWeaver DSL.
```bash
risk-weaver-exe generate-template > custom-risk-weaver.hs
```
4. Update risk definition(riskForGroundTruth and riskForDetection) in the generated template file.
5. Run the RiskWeaver code to generate the output.
```bash
# Build the updated code.
cabal exec ghc custom-risk-weaver.hs
```
Then use "custom-risk-weaver" command instead of risk-weaver-exe.

## Usage

You can reduce the risk of object detection by using the following steps:

1. Define the risk.
2. Run the model and output the result in COCO format.
3. Analyze the risk and visualize it, and if necessary, redefine the risk.
4. Fine-tune the model to reduce the risk.
5. Analyze the risk and visualize it.

### 1. Define the risk

You can define the risk in the custom-risk-weaver.hs file. 
The risk is defined in the riskForGroundTruth and riskForDetection functions to measure recall and precision.
Both functions return a list of Risk objects.

The default risk definition is as follows:
```haskell
  -- | Error type
  data ErrorType _
    = FalsePositive (Set SubErrorType)
    | FalseNegative (Set SubErrorType)
    | TruePositive
    | TrueNegative
    deriving (Ord, Eq, Generic, NFData)

  -- | The definition of risk
  data Risk _ = BddRisk
    { riskType :: ErrorType BoundingBoxGT, -- ^ Error type of the risk like FalsePositive, FalseNegative, TruePositive, TrueNegative
      risk :: Double, -- ^ Risk value for each object
      riskGt :: Maybe BoundingBoxGT, -- ^ The ground truth object of the risk value
      riskDt :: Maybe (Detection BoundingBoxGT) -- ^ The detected object of the risk value
    } deriving (Show, Ord, Eq, Generic, NFData)

  -- | Risk for ground truth: It is used to measure recall.
  riskForGroundTruth :: forall m. (Monad m) => ReaderT (Env BoundingBoxGT) m [Risk BoundingBoxGT]
  riskForGroundTruth = do
    env <- ask
    -- loopG is a function to iterate over all ground truth objects.
    loopG (++) [] $ \(gt :: a) ->
      -- whenInterestAreaG is a function to check if the ground truth object is in the interest area.
      whenInterestAreaG (envUseInterestArea env) gt $ do
        -- riskBias is a factor to adjust the risk value. In this case,
        -- the risk value is adjusted by 10 when the object is a pedestrian.
        let riskBias = if classG @BoundingBoxGT gt == Pedestrian then 10 else 1
        -- detectG finds the detected object corresponding to the ground truth object.
        case detectG env gt of
          -- If the detected object is found, it returns true positive and a low risk value(0.0001).
          Just (dt :: Detection a) ->
            return [BddRisk {riskGt = Just gt, riskDt = Just dt, risk = 0.0001, riskType = TruePositive}]
          -- If the detected object is not found, it returns false negative.
          Nothing -> do
            -- Find the detected object with the maximum IoU.
            case detectMaxIouG env gt of
              -- If the detected object is not found, it returns false negative and a high risk value(30).
              Nothing -> return [BddRisk {riskGt = Just gt, riskDt = Nothing, risk = riskBias * 30, riskType = FalseNegative []}]
              Just (dt :: Detection a) -> do
                case ( classD @BoundingBoxGT dt == classG @BoundingBoxGT gt, -- ^ Class match between ground truth and detection
                       scoreD @BoundingBoxGT dt > confidenceScoreThresh env, -- ^ If the confidence score of detection is higher than the threshold, it returns true.
                       ioU gt dt > ioUThresh env, -- ^ If the IoU of detection is higher than the threshold, it returns true. IoU is Intersection over Union.
                       ioG gt dt > ioUThresh env  -- ^ If the IoG of ground truth is higher than the threshold, it returns true. IoG is Intersection over Ground truth.
                     ) of
                  (False, False, False, True) -> return [BddRisk {riskGt = Just gt, riskDt = Just dt, risk = riskBias * 5.1, riskType = FalseNegative [MissClass, LowScore, Occulusion]}]
                  (False, False, True, _) -> return [BddRisk {riskGt = Just gt, riskDt = Just dt, risk = riskBias * 5, riskType = FalseNegative [MissClass, LowScore]}]
                  (False, True, False, True) -> return [BddRisk {riskGt = Just gt, riskDt = Just dt, risk = riskBias * 5.1, riskType = FalseNegative [MissClass, Occulusion]}]
                  (False, True, True, _) -> return [BddRisk {riskGt = Just gt, riskDt = Just dt, risk = riskBias * 2, riskType = FalseNegative [MissClass]}]
                  (True, False, False, True) -> return [BddRisk {riskGt = Just gt, riskDt = Just dt, risk = riskBias * 5.1, riskType = FalseNegative [LowScore, Occulusion]}]
                  (True, False, True, _) -> return [BddRisk {riskGt = Just gt, riskDt = Just dt, risk = riskBias * 5, riskType = FalseNegative [LowScore]}]
                  (True, True, False, True) -> return [BddRisk {riskGt = Just gt, riskDt = Just dt, risk = riskBias * 0.1, riskType = FalseNegative [Occulusion]}]
                  (True, True, True, _) -> return [BddRisk {riskGt = Just gt, riskDt = Just dt, risk = riskBias * 0.0001, riskType = TruePositive}]
                  (_, _, False, False) -> return [BddRisk {riskGt = Just gt, riskDt = Nothing, risk = riskBias * 30, riskType = FalseNegative []}]
```


### 2. Run the model and output the result in COCO format

Generate a coco result output for an analized model and test dataset.
At first you need to add the following code to generate the output.
```python

```


### 3. Analyze the risk and visualize it, and if necessary, redefine the risk

### 4. Fine-tune the model to reduce the risk

### 5. Analyze the risk and visualize it

```


