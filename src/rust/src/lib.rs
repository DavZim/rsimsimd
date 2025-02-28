use extendr_api::prelude::*;

use simsimd::SpatialSimilarity;
use simsimd::capabilities;

#[extendr]
fn get_capabilities_rs() -> Pairlist {
  Pairlist::from_pairs(&[
    ("genoa", capabilities::uses_genoa()),
    ("haswell", capabilities::uses_haswell()),
    ("ice", capabilities::uses_ice()),
    ("neon", capabilities::uses_neon()),
    ("neon_bf16", capabilities::uses_neon_bf16()),
    ("neon_f16", capabilities::uses_neon_f16()),
    ("neon_i8", capabilities::uses_neon_i8()),
    ("sapphire", capabilities::uses_sapphire()),
    ("sierra", capabilities::uses_sierra()),
    ("skylake", capabilities::uses_skylake()),
    ("sve", capabilities::uses_sve()),
    ("sve_bf16", capabilities::uses_sve_bf16()),
    ("sve_f16", capabilities::uses_sve_f16()),
    ("sve_i8", capabilities::uses_sve_i8()),
    ("turin", capabilities::uses_turin())
  ])
}

// ============================================================================
// Cosine
#[extendr]
fn dist_cosine_rs(vec_a: Vec<f64>, vec_b: Vec<f64>) -> f64 {
  // note cosine calculates the cosine distance, whereas we want to
  // have the cosine similarity
  1.0 - f64::cosine(&vec_a, &vec_b)
    .expect("Vectors must be of the same length")
}

#[extendr]
fn dist_cosine_single_mult_rs(vec: Vec<f64>, ll: List) -> Vec<f64> {
  ll.iter()
  .map(|(_, item)| {
    let vec_b: Vec<f64> = item.clone().try_into().expect("Elements must be numeric vectors");
    1.0 - f64::cosine(&vec, &vec_b).expect("Vector elements must be of the same length")
  })
  .collect()
}


#[extendr]
fn dist_cosine_mult_mult_rs(ll_a: List, ll_b: List) -> Robj {
  let len_a = ll_a.len();
  let len_b = ll_b.len();
  let mut result = Array2::<f64>::zeros((len_a, len_b));

  for (i, (_, item_a)) in ll_a.iter().enumerate() {
    let vec_a: Vec<f64> = item_a.clone().try_into().expect("Elements of ll_a must be numeric vectors");

    for (j, (_, item_b)) in ll_b.iter().enumerate() {
      let vec_b: Vec<f64> = item_b.clone().try_into().expect("Elements of ll_b must be numeric vectors");

      let dist = 1.0 - f64::cosine(&vec_a, &vec_b)
        .expect("Vector elements must be of the same length");

      result[[i, j]] = dist;
    }
  }

  result.try_into().unwrap()
}

#[extendr]
fn dist_cosine_mat_rs(ll: List) -> Robj {
  let len = ll.len();
  let mut result = Array2::<f64>::ones((len, len));

  let vectors: Vec<Vec<f64>> = ll.iter()
    .map(|(_, item)| item.clone().try_into().expect("Elements must be numeric vectors"))
    .collect();

  for i in 0..len {
    for j in 0..i {
      let dist = 1.0 - f64::cosine(&vectors[i], &vectors[j])
        .expect("Vectors must be of the same length");

      result[[i, j]] = dist;
      result[[j, i]] = dist; // Mirror the lower triangle to the upper
    }
  }

  result.try_into().unwrap()
}



// Macro to generate exports.
// This ensures exported functions are registered with R.
// See corresponding C code in `entrypoint.c`.
extendr_module! {
    mod rsimsimd;
    
    fn get_capabilities_rs;
    
    fn dist_cosine_rs;
    fn dist_cosine_single_mult_rs;
    fn dist_cosine_mult_mult_rs;
    fn dist_cosine_mat_rs;


}
