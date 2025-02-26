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
fn dist_cosine_single_mult_rs(vec: Vec<f64>, mat: ArrayView2<f64>) -> Vec<f64> {
  mat.outer_iter()
    .map(|a| 1.0 - f64::cosine(&a.to_vec(), &vec).expect("Vector elements must be of the same length"))
    .collect()
}

#[extendr]
fn dist_cosine_mult_mult_rs(mat_a: ArrayView2<f64>, mat_b: ArrayView2<f64>) -> Robj {
  assert_eq!(mat_a.dim().1, mat_b.dim().1, "Matrices must have the same number of columns");

  let (rows_a, _) = mat_a.dim();
  let (rows_b, _) = mat_b.dim();
  let mut result = Array2::<f64>::zeros((rows_a, rows_b));

  for i in 0..rows_a {
    for j in 0..rows_b {
      let vec_a = mat_a.row(i).to_vec();
      let vec_b = mat_b.row(j).to_vec();

      let dist = 1.0 - f64::cosine(&vec_a, &vec_b)
        .expect("Vectors must be of the same length");

      result[[i, j]] = dist;
    }
  }

  result.try_into().unwrap()
}

#[extendr]
fn dist_cosine_mat_rs(mat: ArrayView2<f64>) -> Robj {
  let (rows, _) = mat.dim();
  let mut result = Array2::<f64>::ones((rows, rows));

  for i in 0..rows {
    for j in 0..i {
      let dist = 1.0 - f64::cosine(&mat.row(i).to_vec(), &mat.row(j).to_vec())
        .expect("Vectors must be of the same length");
      result[[i, j]] = dist;
      result[[j, i]] = dist; // Mirror the lower triangle to upper
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
